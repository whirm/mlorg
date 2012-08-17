(** The parser for blocks *)

val parse : string Batteries.Enum.t -> Block.t list
(** Parse a bunch of lines *)

val parse_lazy : string Batteries.Enum.t -> Block.t BatEnum.t
(** Parse lazily a bunch of lines *)
