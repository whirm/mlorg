(** Automata definition to parse block *)
(**
This file defines generic automaton that will be used to parse the ad-hoc constructions of the org-mode syntax.
The automata definitions may be found in files =aut_*.org=.
*)
open Prelude
open Batteries

(** {1 Type definitions} *)
type input = {
    line : string;
(** The line *)
    context: Context.t;
(** The context *)
    parse : Context.t -> string Enum.t -> Context.t * Block.t list;
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
  val parse_line : state -> input -> (Context.t * state return)
  (** The parsing function. The integer is the number of the line in the stream.
      We give it a context it can modify.  *)

  val interrupt : Context.t -> state 
    -> (Context.t -> string Enum.t -> Context.t * Block.t list) 
    -> Context.t * Block.t list
  (** - The interruption function, called when the parser decides to switch
      over to another automaton. *)
    
  val priority : int
  (** The priority of the automaton. When an automaton
      says it is partially done, the parser will look for accepting automaton with
      higher priority. *)
    
  val is_start : input -> (Context.t * state) option
(** The following function, on a line returns [Some initial_state] if it could
    be the beginning of a valid input for the automaton, [None] otherwise. *)
end

type t = (module Automaton)
(** The type of automata *)

val sort : t list -> t list
(** Sort a list of automata by priorities *)

