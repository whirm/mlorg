(** The parser for blocks *)

val parse : string Batteries.Enum.t -> Block.t list
(** Parse a bunch of lines *)
