open Cobalt

let create_deliver_cb me =
  let deliver_cb us who msg =
    Printf.printf "%s received message %s from %s\n" us msg who
  in
  deliver_cb me



let u1 = CB.init ["ena"; "dio"; "tria"] "ena"  (create_deliver_cb "ena");;
let u2 = CB.init ["ena"; "dio"; "tria"] "dio"  (create_deliver_cb "dio");;
let u3 = CB.init ["ena"; "dio"; "tria"] "tria" (create_deliver_cb "tria");;

let (u1, m1) = CB.broadcast u1 "ti leei";;

let u2 = CB.receive u2 "ena" m1;;
(*
Printf.printf "when only u2 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;
*)

let (u2, m2) = CB.broadcast u2 "ola kala re";;
let u1 = CB.receive u1 "ena" m2;;
(*
Printf.printf "when only u2 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;
*)

let u3 = CB.receive u3 "dio" m2;;
(*
Printf.printf "when u3 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;
*)

let u3 = CB.receive u3 "ena" m1;;
(*
Printf.printf "when only u3 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;
*)
