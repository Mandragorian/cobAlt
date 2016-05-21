module CB :
  sig
    type state (* = {
      users : string list;
      me    : string;
      requests_queue : (string * RequestSet.t) array;
      current_requester : int;
      delivered : Message.t IdMap.t;
      undelivered : Message.t IdMap.t;
      frontier : IdSet.t;
      wire : Message.t IdMap.t;
    }*)
    val init : string list -> string -> state
    val broadcast : state -> string -> state * string
    val receive : state -> string -> string -> state
    val how_many_delivered : state -> int
    val how_many_undelivered : state -> int
  end
