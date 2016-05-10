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

type request =
    | Outgoing of Message.id_t
    | LostMsg of Message.id_t
    | RetransmitMsg of Message.id_t


module Request =
    struct
        type t = request
        let compare x y =
            match x with
            | Outgoing id_x | LostMsg id_x | RetransmitMsg id_x ->
                    match y with
                    | Outgoing id_y | LostMsg id_y | RetransmitMsg id_y ->
                            Message.compare_ids id_x id_y
        let create_outgoing id =
           Outgoing id
        let create_lost id =
            LostMsg id

    end

module RequestSet = Set.Make(Request)

module CB =
    struct
        type state = {
            users : string list;
            me    : string;
            requests_queue : (string * RequestSet.t) array;
            delivered : Message.t IdMap.t;
            undelivered : Message.t IdMap.t;
            frontier : IdSet.t;
            wire : Message.t IdMap.t;
        }
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
            let us = add_request us us.me (Request.create_outgoing (Message.id msg)) in
            (us, msg)

    end





module Sendable =
    struct
        type t =
            | Msg of Message.t
            | Req of Request.t
            | None

        let create_sendable_message msg =
            Msg msg

        let create_sendable_request req =
            Req req

        let none =
            None

        let receive us who = function
            | Msg msg ->
                let parents_delivered state msg =
                    let check_parent par parents_delivered_flag =
                        parents_delivered_flag && (IdMap.mem par (CB.delivered state))
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
                            let new_undelivered = IdMap.remove m_id (CB.undelivered st) in
                            let new_st = CB.update_undelivered st new_undelivered in
                            let new_st = CB.deliver new_st m in
                            (new_st, true)
                        else
                            (st,f)
                    in
                    let (new_state, delivered_flag) = IdMap.fold deliver_any
                                                        (CB.undelivered state)
                                                        (state,false) in
                    if delivered_flag then
                        delivery_loop new_state
                    else
                        new_state
                in
                let add_all_requests e st =
                    CB.add_request st (CB.me st) (Request.create_lost e)
                in
                let new_us = CB.remove_request us who (Request.create_lost (Message.id msg)) in
                let parents_set = Message.parents msg in
                let received_msgs = List.map (fun (k,_) -> k)
                                        (List.rev_append
                                            (IdMap.bindings (CB.delivered us))
                                            (IdMap.bindings (CB.undelivered us))
                                        )
                in
                let dangling_parents = IdSet.diff parents_set (IdSet.of_list received_msgs)  in
                let new_us = IdSet.fold add_all_requests dangling_parents new_us in
                let new_undelivered = IdMap.add (Message.id msg) msg (CB.undelivered new_us) in
                let new_wire = IdMap.add (Message.id msg) msg (CB.wire new_us) in
                let new_us = CB.update_wire new_us new_wire in
                let new_us = CB.update_undelivered new_us new_undelivered in
                let new_us = delivery_loop new_us in
                new_us
            |_ -> raise (Not_implemented "Only message receive is implemented")

    end






let fullfil us who = function
    | Outgoing id ->
            let msg = IdMap.find id (CB.delivered us) in
            let new_wire = IdMap.add id msg (CB.wire us) in
            let new_us = CB.update_wire us new_wire in
            (new_us, Sendable.create_sendable_message msg, CB.users us)
    | LostMsg id ->
            let new_us = CB.add_request us who (LostMsg id) in
            let to_send = Sendable.create_sendable_request (LostMsg id) in
            (new_us, to_send, CB.users us)
    | RetransmitMsg id ->
            try
                let msg = IdMap.find id (CB.wire us) in
                let to_send = Sendable.create_sendable_message msg in
                (us, to_send, [who])
            with Not_found -> (us, Sendable.none, [])
    | _ -> raise (Not_implemented "this request type is not implemented")






let u1 = CB.init ["ena"; "dio"; "tria"] "ena";;
let u2 = CB.init ["ena"; "dio"; "tria"] "dio";;
let u3 = CB.init ["ena"; "dio"; "tria"] "tria";;

let (u1, m1) = CB.broadcast u1 "ti leei";;
let first_from_u1 = Sendable.create_sendable_message m1;;

let u2 = Sendable.receive u2 "ena" first_from_u1;;

Printf.printf "when only u2 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;

let (u1, m2) = CB.broadcast u1 "ti leei";;
let second_from_u1 = Sendable.create_sendable_message m2;;

let u2 = Sendable.receive u2 "ena" second_from_u1;;

Printf.printf "when only u2 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;


let u3 = Sendable.receive u3 "ena" second_from_u1;;

Printf.printf "when u3 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;

let u3 = Sendable.receive u3 "ena" first_from_u1;;

Printf.printf "when only u3 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u1));;
Printf.printf "u1 delivered num = %d\n" (IdMap.cardinal (CB.delivered u1));;
Printf.printf "u2 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u2));;
Printf.printf "u2 delivered num = %d\n" (IdMap.cardinal (CB.delivered u2));;
Printf.printf "u3 undelivered num = %d\n" (IdMap.cardinal (CB.undelivered u3));;
Printf.printf "u3 delivered num = %d\n" (IdMap.cardinal (CB.delivered u3));;

