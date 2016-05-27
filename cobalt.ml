let find a pred =
    let rec aux a pred n =
        if  pred a.(n)  then n
        else aux a pred (n+1)
    in
    aux a pred 0

exception Not_implemented of string
exception Not_request of string
exception Malformed_message of string

(* This module is an abstraction of the messages sent by the
 * protocol. When a user types a message a Message.t will be
 * created and serialized. This serialized form is what will
 * actually be sent over the network *)
module Message =
    struct
        (* The type of the id of each message *)
        type id_t = string
        (* This is a mapping from IDs to messages *)
        module IdMap = Map.Make(
            struct
                type t = id_t
                let compare = compare
            end)
        (* And this is a set containing (unique) message IDs *)
        module IdSet = Set.Make(
            struct
                type t = id_t
                let compare = compare
            end);;
        (* The type of the messages *)
        type t = {
            author : string;
            parents : IdSet.t;
            payload : string;
            id : id_t
        }
        (* A comparison function for messages *)
        let compare x y =
            String.compare x.id y.id
        (* Serialize a message *)
        let serialize msg =
          (* Get the parents (causal ancestors) of the message
           * and encode them in base64 *)
          let parent_list = IdSet.elements msg.parents in
          let parent_list = List.map (fun x -> Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string x)))  parent_list in
          (* Serialize them in the form <parent_a>.<parent_b>.<parent_c>. ... *)
          let parents_serialized = if (List.length parent_list) != 0 then String.concat "." parent_list else " " in
          (* Encode the id of the message in base64 *)
          let encoded_id = Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string msg.id)) in
          (*And then serialize the whole message in the form:
           * <msg_tag>:<msg_author>:<msg_payload>:<msg_id>:<serialized_parent_list> *)
          let serialized = String.concat ":" ["??MSG"; msg.author; msg.payload; encoded_id; parents_serialized] in
          serialized
        (* Parse a serialized message *)
        let parse msg =
          (* Helper function that decodes a base64 encoded value *)
          let decode encoded =
            let id = (Nocrypto.Base64.decode (Cstruct.of_string encoded)) in
            match id with
            | None ->
              raise (Malformed_message "Badly formed id in message")
            | Some some_id ->
              Cstruct.to_string some_id
          in
          (* Two regular expressions matching the field separators ( : and . )*)
          let fields_sep = Str.regexp ":" in
          let parents_sep = Str.regexp "\\." in
          (* Split the message at the : separator *)
          let splitted_msg = Str.split fields_sep msg in
          (* And get each value. TODO there is probably a better way to do this
           * that does not produce compiler warnings *)
          let _ :: author :: payload :: id :: [parents_serialized] = splitted_msg in
          let decoded_id = decode id in
          (* Split the parent list at the . separator *)
          let parents_serialized = (Str.split parents_sep parents_serialized ) in
          (* Check if the message has any parents. If no return the empty list.
           * If yes decode each parent in the parent list and return them in a list *)
          let decoded_parents_serialized =
            if List.length parents_serialized = 1 && List.hd parents_serialized = " " then
              []
            else
              List.map (fun x -> decode x) parents_serialized
          in
          (* Using the above list create the parents set *)
          let parents = IdSet.of_list decoded_parents_serialized in
          {
            author = author;
            parents = parents;
            payload = payload;
            id = decoded_id
          }
        (* A function to compare IDs *)
        let compare_ids =
            String.compare
        (* This function accepts a message and creates its ID by hashing
         * all the information contained in the message *)
        let create_id s =
            let md = Nocrypto.Hash.SHA512.init() in
            let inp = Cstruct.of_string s in
            let () = Nocrypto.Hash.SHA512.feed md inp in
            let digest_cs = Nocrypto.Hash.SHA512.get md in
            Cstruct.to_string digest_cs
        (* This function creates a message from its author, parents and contents *)
        let create who causes mesg =
            let elt_list = IdSet.elements causes in
            let id_info = String.concat "" (who :: mesg :: elt_list ) in
            let msg_id = create_id id_info in
            {
                author = who;
                parents = causes;
                payload = mesg;
                id = msg_id;
            }
        (* This function simply returns the message id *)
        let id msg =
            msg.id
        (* This function simply returns the message parents *)
        let parents msg =
            msg.parents
        let author msg =
          msg.author
        let payload msg =
          msg.payload
    end

(* TODO this is probably not a good programming practice.
 * Maybe refactor the code and change how Message an CB
 * interract with each other *)
module IdMap = Message.IdMap
module IdSet = Message.IdSet


(* This is the module that implements the oldblue protocol.*)
module CB =
    struct
        (* A type for the requests *)
        type request =
          | Outgoing of Message.id_t
          | LostMsg of Message.id_t
          | RetransmitMsg of Message.id_t
          | No_req
        (* This functions create a request according to their name *)
        let create_outgoing id =
          Outgoing id
        let create_lost id =
          LostMsg id
        (* Serialize a request *)
        let request_serialize r =
          (* A helper function to encode a value to base64 *)
          let encode id = Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string id)) in
          (* To serialize a request just prepend the letters O L or R according
           * to the request type and then append the encoded id of the message
           * that this request is referencing *)
          match r with
          | Outgoing id -> "O" ^ (encode id)
          | LostMsg id -> "L" ^ (encode id)
          | RetransmitMsg id -> "R" ^ (encode id)
          | No_req -> raise (Not_request "tried to serialize a None request")
        (* This function parses a serialized message *)
        let request_parse r =
          (* A helper function to decode a base6 encoded value *)
          let decode encoded =
            let id = (Nocrypto.Base64.decode (Cstruct.of_string encoded)) in
            match id with
            | None ->
              raise (Malformed_message "Badly formed id in message")
            | Some some_id ->
              Cstruct.to_string some_id
          in
          let len = String.length r in
          let id = String.sub r 1 (len - 1) in
          match r.[0] with
          | 'O' -> Outgoing (decode id)
          | 'L' -> LostMsg (decode id)
          | 'R' -> RetransmitMsg (decode id)
          | _ -> raise (Not_request "received request is of unknown request type")
        (* A set that will contain all the requests by the ID of the
         * message they are referencing *)
        module RequestSet = Set.Make(struct
          type t = request
          let compare x y =
            match x with
            | Outgoing id_x | LostMsg id_x | RetransmitMsg id_x ->
              (match y with
                | Outgoing id_y | LostMsg id_y | RetransmitMsg id_y ->
                    Message.compare_ids id_x id_y
                | No_req ->
                    raise (Not_request "tried to compare None request")
              )
            | No_req -> raise (Not_request "tried to compare None request")
        end)
        (* The type of the user state as defined in the oldblue protocol*)
        type state = {
            users : string list;
            me    : string;
            requests_queue : (string * RequestSet.t) array;
            current_requester : int;
            delivered : Message.t IdMap.t;
            undelivered : Message.t IdMap.t;
            frontier : IdSet.t;
            wire : Message.t IdMap.t;
            on_delivery : string -> string -> unit;
        }
        (* A type encapsulating what the protocol can send. Either a message
         * or a request *)
        type sendable_t =
            | Msg of Message.t
            | Req of request
            | None
        (* This function parses a serialised sendable. TODO maybe rename to
         * parse? *)
        let sendable_of_string s =
          let msg_regexp = Str.regexp "\\?\\?MSG" in
          let req_regexp = Str.regexp "\\?\\?REQ" in
          let msg_tag_pos = Str.string_match msg_regexp s 0 in
          if msg_tag_pos then
            Msg (Message.parse s)
          else
            let req_tag_pos = Str.string_match req_regexp s 0 in
            if req_tag_pos then
              Req (request_parse s)
            else None
        (* Thisf function serializes a sendable. TODO maybe rename to
         * serialize? *)
        let string_of_sendable = function
          | Msg msg -> Some (Message.serialize msg)
          | Req req -> Some (request_serialize req)
          | None -> None
        (* This function creates a new user state from the participants username list
         * and the username of our user *)
        let init usrs username delivery_cb =
            let req_list = List.map (fun x -> (x, RequestSet.empty)) usrs in
            let opened = {
                users = usrs;
                me = username;
                requests_queue = Array.of_list req_list;
                current_requester = 0;
                delivered = IdMap.empty;
                undelivered = IdMap.empty;
                frontier = IdSet.empty;
                wire = IdMap.empty;
                on_delivery = delivery_cb;
            } in
            opened
        (* The following function is only for testing remove this in
        * "production" *)
        let how_many_delivered s =
          IdMap.cardinal s.delivered
        let how_many_undelivered s =
          IdMap.cardinal s.undelivered
        (***********************************************************)

        (* These functions update each field of a user state *)
        let update_wire state w =
          {state with wire = w}
        let update_delivered state d =
          {state with delivered = d}
        let update_undelivered state u =
          {state with undelivered = u}
        let update_frontier state f =
          {state with frontier = f}
        let update_current_requester state cr =
          {state with current_requester = cr}
        (*****************************************************)

        (* These functions return the value of the corresponding field *)
        let users us =
            us.users
        let me us =
            us.me
        let requests us =
            us.requests_queue
        let delivered us =
            us.delivered
        let undelivered us =
            us.undelivered
        let wire us =
            us.wire
        let frontier us =
            us.frontier
        let current_requester us =
          us.current_requester
        (***************************************************************)

        (* This function delivers a message as specified in the protocol *)
        let deliver us msg =
            let new_delivered = IdMap.add (Message.id msg) msg us.delivered in
            let inter_front = IdSet.diff us.frontier (Message.parents msg) in
            let new_front = IdSet.add (Message.id msg) inter_front  in
            let new_us = update_delivered us new_delivered in
            let new_us = update_frontier new_us new_front in
            new_us.on_delivery (Message.author msg) (Message.payload msg);
            new_us
        (* This function adds a request in the requst queue *)
        let add_request us auth req =
            let find_pred (orig, _ ) = orig = auth in
            let pos = find  us.requests_queue find_pred in
            let ( _, reqs) = us.requests_queue.(pos) in
            let new_reqs = RequestSet.add req reqs in
            us.requests_queue.(pos) <- (auth, new_reqs);
            us
        (* And this function removes a request from the queue *)
        let remove_request us auth req =
            let find_pred (orig, _ ) = orig = auth in
            let pos = find us.requests_queue find_pred in
            let ( _, reqs) = us.requests_queue.(pos) in
            let new_reqs = RequestSet.remove req reqs in
            us.requests_queue.(pos) <- (auth, new_reqs);
            us
        (* This function is a wrapper for Message.create *)
        let create_message auth msg front =
            let msg = Message.create auth front msg
            in
            msg
        (* This is an API endpoint, used by the application when a message is transmitted *)
        let broadcast us msg =
            let msg = create_message us.me msg us.frontier in
            let us = deliver us msg in
            let us = add_request us us.me (create_outgoing (Message.id msg)) in
            (us, Message.serialize msg)
        (* This is an API endpoint, used by the application when a message is received *)
        let receive us who received =
          (* Check if we received a message or a request. Currently only
           * messages can be received and not requests. *)
          match (sendable_of_string received) with
            (* If we received a message procces it *)
            | Msg msg ->
                (* A helper function that checks if all of the message's
                 * parents are delivered *)
                let parents_delivered state msg =
                    let check_parent par parents_delivered_flag =
                        parents_delivered_flag && (IdMap.mem par (delivered state))
                    in
                    let parnts = Message.parents msg in
                    if IdSet.is_empty parnts then
                        true
                    else
                        IdSet.fold check_parent parnts true
                in
                (* A helper function that delivers any deliverable message
                 * until no new messages can be delivered *)
                let rec delivery_loop state =
                    let deliver_any m_id m (st, f) =
                        if parents_delivered st m then
                            let new_undelivered = IdMap.remove m_id (undelivered st) in
                            let new_st = update_undelivered st new_undelivered in
                            let new_st = deliver new_st m in
                            (new_st, true)
                        else
                            (st,f)
                    in
                    let (new_state, delivered_flag) = IdMap.fold deliver_any
                                                        (undelivered state)
                                                        (state,false) in
                    if delivered_flag then
                        delivery_loop new_state
                    else
                        new_state
                in
                (* A helper function that adds a lost requst for message e
                 * in the state st *)
                let add_all_requests e st =
                    add_request st (me st) (create_lost e)
                in
                (* Remove any LostMsg request referencing the message just
                 * received *)
                let new_us = remove_request us who (create_lost (Message.id msg)) in
                (* Get the message parents *)
                let parents_set = Message.parents msg in
                (* Get all received messages, delivered or not *)
                let received_msgs = List.map (fun (k,_) -> k)
                                        (List.rev_append
                                            (IdMap.bindings (delivered us))
                                            (IdMap.bindings (undelivered us))
                                        )
                in
                (* Get any parents that we have not received *)
                let dangling_parents = IdSet.diff parents_set (IdSet.of_list received_msgs)  in
                (* Add a lost message request for each dangling parent *)
                let new_us = IdSet.fold add_all_requests dangling_parents new_us in
                (* Add the message to the undelivered Map *)
                let new_undelivered = IdMap.add (Message.id msg) msg (undelivered new_us) in
                (* Add the message in the wire for possible retransmission *)
                let new_wire = IdMap.add (Message.id msg) msg (wire new_us) in
                let new_us = update_wire new_us new_wire in
                let new_us = update_undelivered new_us new_undelivered in
                (* Deliver any message that can be delivered *)
                let new_us = delivery_loop new_us in
                new_us
            |_ -> raise (Not_implemented "Only message receive is implemented")
        (* This function is an API endpoint used to fullfill a scheduled request.
          * It is not well integrated and should not be used yet *)
        let fullfil us who req_s =
          let req = request_parse req_s in
          match req with
          | Outgoing id ->
            let msg = IdMap.find id (delivered us) in
            let new_wire = IdMap.add id msg (wire us) in
            let new_us = update_wire us new_wire in
            (new_us, Message.serialize msg, users us)
          | LostMsg id ->
            let new_us = add_request us who (LostMsg id) in
            let to_send = request_serialize (LostMsg id) in
            (new_us, to_send, users us)
          | RetransmitMsg id ->
            (try
              let msg = IdMap.find id (wire us) in
              let to_send = Message.serialize msg in
              (us, to_send, [who])
            with Not_found -> (us, "", []))
          | _ -> raise (Not_implemented "this request type is not implemented")
        (* This is an API endpoint used to schedule the requests. It is not
          * well integrated and should no be used yet *)
        let schedule us =
          let cr_r = ref (current_requester us) in
          let who_r = ref "" in
          let rqueue_r = ref RequestSet.empty in
          let request_r : request ref = ref No_req in
          let stop_while_r = ref false in
          let () =
            while not !stop_while_r do
              try
                let (who, rqueue) = (requests us).(!cr_r) in
                request_r := RequestSet.choose rqueue;
                who_r := who;
                rqueue_r := rqueue;
                stop_while_r := true
              with Not_found ->
                cr_r := (!cr_r + 1) mod (List.length (users us))
            done
          in
          let new_rqueue = RequestSet.remove (!request_r) (!rqueue_r) in
          let () = us.requests_queue.(!cr_r) <- (!who_r, new_rqueue) in
          let new_us = {us with current_requester = (!cr_r + 1) mod (List.length (users us))} in
          (new_us, !who_r, !request_r)
    end


