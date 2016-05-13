let find a pred =
    let rec aux a pred n =
        if  pred a.(n)  then n
        else aux a pred (n+1)
    in
    aux a pred 0


let inp = Cstruct.of_string "asdf";;

let hash = Nocrypto.Hash.SHA512.init() in
let () = Nocrypto.Hash.SHA512.feed hash inp in
let cs = Nocrypto.Hash.SHA512.get hash in
let bcs = Nocrypto.Base64.encode cs in
let stri = Cstruct.to_string bcs in
print_endline stri;;

exception Not_implemented of string
exception Not_request of string
exception Malformed_message of string
module Message =
    struct
        type id_t = string
        module IdMap = Map.Make(
            struct
                type t = id_t
                let compare = compare
            end)

        module IdSet = Set.Make(
            struct
                type t = id_t
                let compare = compare
            end);;

        type t = {
            author : string;
            parents : IdSet.t;
            payload : string;
            id : id_t
        }
        let compare x y =
            String.compare x.id y.id
        let serialize msg =
          let parent_list = IdSet.elements msg.parents in
          let parent_list = List.map (fun x -> Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string x)))  parent_list in
          let parents_serialized = if (List.length parent_list) != 0 then String.concat "." parent_list else " " in
          let encoded_id = Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string msg.id)) in
          let serialized = String.concat ":" ["??MSG"; msg.author; msg.payload; encoded_id; parents_serialized] in
          serialized
        let parse msg =
          let decode encoded =
            let id = (Nocrypto.Base64.decode (Cstruct.of_string encoded)) in
            match id with
            | None ->
              raise (Malformed_message "Badly formed id in message")
            | Some some_id ->
              Cstruct.to_string some_id
          in
          let fields_sep = Str.regexp ":" in
          let parents_sep = Str.regexp "\\." in
          let splitted_msg = Str.split fields_sep msg in
          let _ :: author :: payload :: id :: [parents_serialized] = splitted_msg in
          let decoded_id = decode id in
          let parents_serialized = (Str.split parents_sep parents_serialized ) in
          let decoded_parents_serialized =
            if List.length parents_serialized = 1 && List.hd parents_serialized = " " then
              []
            else
              List.map (fun x -> decode x) (*let Some dec_x_cs = (Nocrypto.Base64.decode (Cstruct.of_string x)) in Cstruct.to_string dec_x_cs *) parents_serialized
          in
          let parents = IdSet.of_list decoded_parents_serialized in
          {
            author = author;
            parents = parents;
            payload = payload;
            id = decoded_id
          }
        let compare_ids =
            String.compare
        let create_id s =
            let md = Nocrypto.Hash.SHA512.init() in
            let inp = Cstruct.of_string s in
            let () = Nocrypto.Hash.SHA512.feed md inp in
            let digest_cs = Nocrypto.Hash.SHA512.get md in
            Cstruct.to_string digest_cs
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

        let id msg =
            msg.id

        let parents msg =
            msg.parents
    end
module IdMap = Message.IdMap
module IdSet = Message.IdSet

module CB =
    struct
        type request =
          | Outgoing of Message.id_t
          | LostMsg of Message.id_t
          | RetransmitMsg of Message.id_t
          | None
        type t = request
        let create_outgoing id =
          Outgoing id
        let create_lost id =
          LostMsg id
        let request_serialize = function
          | Outgoing id -> "O" ^ id
          | LostMsg id -> "L" ^ id
          | RetransmitMsg id -> "R" ^ id
          | None -> raise (Not_request "tried to serialize a None request")
        let request_parse r =
          let len = String.length r in
          let id = String.sub r 1 (len - 1) in
          match r.[0] with
          | 'O' -> Outgoing id
          | 'L' -> LostMsg id
          | 'R' -> RetransmitMsg id
          | _ -> raise (Not_request "received request is of unknown request type")
        module RequestSet = Set.Make(struct
          type t = request
          let compare x y =
            match x with
            | Outgoing id_x | LostMsg id_x | RetransmitMsg id_x ->
              (match y with
                | Outgoing id_y | LostMsg id_y | RetransmitMsg id_y ->
                    Message.compare_ids id_x id_y
                | None ->
                    raise (Not_request "tried to compare None request")
              )
            | None -> raise (Not_request "tried to compare None request")
        end)
        type state = {
            users : string list;
            me    : string;
            requests_queue : (string * RequestSet.t) array;
            delivered : Message.t IdMap.t;
            undelivered : Message.t IdMap.t;
            frontier : IdSet.t;
            wire : Message.t IdMap.t;
        }
        type sendable_t =
            | Msg of Message.t
            | Req of request
            | None
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

        let string_of_sendable = function
          | Msg msg -> Some (Message.serialize msg)
          | Req req -> Some (request_serialize req)
          | None -> None

        let init usrs username =
            let req_list = List.map (fun x -> (x, RequestSet.empty)) usrs in
            let opened = {
                users = usrs;
                me = username;
                requests_queue = Array.of_list req_list;
                delivered = IdMap.empty;
                undelivered = IdMap.empty;
                frontier = IdSet.empty;
                wire = IdMap.empty;
            } in
            opened
        let update_wire state w =
            {
                users = state.users;
                me = state.me;
                requests_queue = state.requests_queue;
                delivered = state.delivered;
                undelivered = state.undelivered;
                frontier = state.frontier;
                wire = w;
            }
        let update_delivered state d =
            {
                users = state.users;
                me = state.me;
                requests_queue = state.requests_queue;
                delivered = d;
                undelivered = state.undelivered;
                frontier = state.frontier;
                wire = state.wire;
            }
        let update_undelivered state u =
            {
                users = state.users;
                me = state.me;
                requests_queue = state.requests_queue;
                delivered = state.delivered;
                undelivered = u;
                frontier = state.frontier;
                wire = state.wire;
            }
        let update_frontier state f =
            {
                users = state.users;
                me = state.me;
                requests_queue = state.requests_queue;
                delivered = state.delivered;
                undelivered = state.undelivered;
                frontier = f;
                wire = state.wire;
            }
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
        let deliver us msg =
            (*let (Id str_msg_id) = msg.id in*)
            let new_delivered = IdMap.add (Message.id msg) msg us.delivered in
            let inter_front = IdSet.diff us.frontier (Message.parents msg) in
            let new_front = IdSet.add (Message.id msg) inter_front  in
            let new_us = update_delivered us new_delivered in
            let new_us = update_frontier new_us new_front in
            new_us
        let add_request us auth req =
            let find_pred (orig, _ ) = orig = auth in
            let pos = find  us.requests_queue find_pred in
            let ( _, reqs) = us.requests_queue.(pos) in
            let new_reqs = RequestSet.add req reqs in
            us.requests_queue.(pos) <- (auth, new_reqs);
            us
        let remove_request us auth req =
            let find_pred (orig, _ ) = orig = auth in
            let pos = find us.requests_queue find_pred in
            let ( _, reqs) = us.requests_queue.(pos) in
            let new_reqs = RequestSet.remove req reqs in
            us.requests_queue.(pos) <- (auth, new_reqs);
            us
        let create_message auth msg front =
            let msg = Message.create auth front msg
            in
            msg
        let broadcast us msg =
            let msg = create_message us.me msg us.frontier in
            let us = deliver us msg in
            let us = add_request us us.me (create_outgoing (Message.id msg)) in
            (us, Message.serialize msg)
        let receive us who received =
          match (sendable_of_string received) with
            | Msg msg ->
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
                let add_all_requests e st =
                    add_request st (me st) (create_lost e)
                in
                let new_us = remove_request us who (create_lost (Message.id msg)) in
                let parents_set = Message.parents msg in
                let received_msgs = List.map (fun (k,_) -> k)
                                        (List.rev_append
                                            (IdMap.bindings (delivered us))
                                            (IdMap.bindings (undelivered us))
                                        )
                in
                let dangling_parents = IdSet.diff parents_set (IdSet.of_list received_msgs)  in
                let new_us = IdSet.fold add_all_requests dangling_parents new_us in
                let new_undelivered = IdMap.add (Message.id msg) msg (undelivered new_us) in
                let new_wire = IdMap.add (Message.id msg) msg (wire new_us) in
                let new_us = update_wire new_us new_wire in
                let new_us = update_undelivered new_us new_undelivered in
                let new_us = delivery_loop new_us in
                new_us
            |_ -> raise (Not_implemented "Only message receive is implemented")
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

    end

(*
*)





let u1 = CB.init ["ena"; "dio"; "tria"] "ena";;
let u2 = CB.init ["ena"; "dio"; "tria"] "dio";;
let u3 = CB.init ["ena"; "dio"; "tria"] "tria";;

let (u1, m1) = CB.broadcast u1 "ti leei";;
print_endline m1;;
let u2 = CB.receive u2 "ena" m1;;
Printf.printf "when only u2 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;
let (u1, m2) = CB.broadcast u1 "ti leei";;
let u2 = CB.receive u2 "ena" m2;;

Printf.printf "when only u2 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;


let u3 = CB.receive u3 "ena" m2;;

Printf.printf "when u3 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;

let u3 = CB.receive u3 "ena" m1;;

Printf.printf "when only u3 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;
