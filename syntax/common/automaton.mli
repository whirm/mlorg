(** Automata definition to parse block *)

(** {!Block} parsing is done using custom automata which can carry its own state.

    A syntax is then a list of automata from which the {!Parser} module create a full parser which
    try every automata in the right order.

    Since this is a common module, it is parametrized over a global type, the type of [contexts]
    which represents the global state of the parser, passed along the different automata *)
open Prelude
open Batteries
module Make (C : sig type context end) : sig
  open C
(** {1 Type definitions} *)
  type input = {
    line : string;
    (** The line *)
    context: context;
    (** The context *)
    parse : context -> string Enum.t -> context * Block.t list;
  (** A parsing functions, for blocks that can contain blocks (lists for instance) *)
  }
(** The input that will be given to automata. *)

  type 'state return = 
    | Next of 'state
  (** Everything is fine, let me continue my work, uninterrupted. *)
    | Partial of 'state
  (** Everything is fine, but I can be interrupted. *)
    | Done of Block.t list * bool
(** I am done. The boolean tells whether the line should be dropped or not. *)
(** This is what an automaton may return on an input *)

(** An automaton will be a module, because we want automaton to be able to have
    different type for their states. *)

  module type Automaton = sig
    type state
  (** The internal state of the automaton *)
    val parse_line : state -> input -> (context * state return)
  (** The parsing function. The integer is the number of the line in the stream.
      We give it a context it can modify.  *)

    val interrupt : context -> state 
      -> (context -> string Enum.t -> context * Block.t list) 
      -> context * Block.t list
  (** The interruption function, called when the parser decides to switch
      over to another automaton. *)
      
    val priority : int
  (** The priority of the automaton. When an automaton
      says it is partially done, the parser will look for accepting automaton with
      higher priority. *)
      
    val is_start : input -> (context * state) option
(** The following function, on a line returns [Some initial_state] if it could
    be the beginning of a valid input for the automaton, [None] otherwise. *)
  end

  type t = (module Automaton)
(** The type of automata *)

  val sort : t list -> t list
(** Sort a list of automata by priorities *)

end
