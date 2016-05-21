open Cobalt

let u1 = CB.init ["ena"; "dio"; "tria"] "ena";;
let u2 = CB.init ["ena"; "dio"; "tria"] "dio";;
let u3 = CB.init ["ena"; "dio"; "tria"] "tria";;

let (u1, m1) = CB.broadcast u1 "ti leei";;

print_endline m1;;
let u2 = CB.receive u2 "ena" m1;;
Printf.printf "when only u2 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;
let (u1, m2) = CB.broadcast u1 "ti leei";;
let u2 = CB.receive u2 "ena" m2;;

Printf.printf "when only u2 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;


let u3 = CB.receive u3 "ena" m2;;

Printf.printf "when u3 has received the second message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;

let u3 = CB.receive u3 "ena" m1;;

Printf.printf "when only u3 has received the first message from u1\n";;
Printf.printf "u1 undelivered num = %d\n" (CB.how_many_undelivered u1);;
Printf.printf "u1 delivered num = %d\n" (CB.how_many_delivered u1);;
(*Printf.printf "u1 requests num = %d\n" (CB.RequestSet.cardinal (CB.requests u1));;*)
Printf.printf "u2 undelivered num = %d\n" (CB.how_many_undelivered u2);;
Printf.printf "u2 delivered num = %d\n" (CB.how_many_delivered u2);;
Printf.printf "u3 undelivered num = %d\n" (CB.how_many_undelivered u3);;
Printf.printf "u3 delivered num = %d\n" (CB.how_many_delivered u3);;


