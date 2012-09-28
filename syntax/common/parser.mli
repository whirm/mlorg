(** The parser for blocks *)
module Make (C : sig type context
                     val number : context -> int
                     val set_number : context -> int -> context
             end) : sig
  open C
  module Automaton : module type of (Automaton.Make (C))
  val parse : Automaton.t list -> context -> string Batteries.Enum.t -> context *  Block.t list
  (** Parse a bunch of lines *)

  val parse_lazy : Automaton.t list -> context -> string Batteries.Enum.t -> Block.t BatEnum.t
  (** Parse lazily a bunch of lines *)

end
(** As for {!Automaton}, this module is actually a functor
    expecting a context type. On top of that,
    we ask that context has a method number that can be retrieved and set *)
