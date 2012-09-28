(** Parser for Org *)

(** As for {!Automaton}, this module is actually a functor
    expecting a context type. On top of that,
    we ask that context has a method number that can be retrieved and set *)

val parse : string Batteries.Enum.t -> Org_context.t *  Block.t list
(** Parse a bunch of lines *)
  
val parse_lazy : string Batteries.Enum.t -> Block.t BatEnum.t
(** Parse lazily a bunch of lines *)
