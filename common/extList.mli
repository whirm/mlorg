(** Extendable lists *)

(** This module deals with extensions. Often we have a list of possible syntaxic
    formation, and we want the user to easily add one that is to be dynlinked to the
    program.

    This module defines what is an extendable list.

    This is done through /extendable lists/. An extendable list is a list that
    extensions may complete.

    An extendable list will be a module:
*)

module type ExtList = sig
  type elt
  (** The type of the elements in the list *)

  val push : elt -> unit
  (** A function to push an element at the beginning of the list *)
  
  val get : unit -> elt list
  (** A function to retrieve the contents of the *)
  
  val update : (elt list -> elt list) -> unit
  (** A general function to modify the contents of the list *)

end

(** To make such lists, a functor hiding a reference, expecting a type and a
    base list is presented. *)

module Make (S : sig type t val base : t list end) : ExtList with type elt = S.t
(** This functor takes a type for the list element and a base list (elements
    specified by the programer.  After that user may add elements with the
    function [push] or [update] *)
