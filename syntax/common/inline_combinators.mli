(** Combinators for easy Inline parsing *)

(** {1 Parsers} *)
(** This section deals with defining what a parser for a construction is. *)

type 'a parser = (string -> 'a list) -> BatSubstring.t -> ('a list * BatSubstring.t) option
  (** A parser simply a function taking a [BatSubstring.t] and returning an option of
      [t list]. The parser takes an extra parameter, a function to parse a
      string and return a list of tokens ['a]. The function may as well throw an
      exception.
  *)

val run_parsers : (string -> 'a) -> 'a parser list -> string -> 'a list
(** Run a list of parsers over a string and returned the generated contents.
    Note that the order matter : first parsers have higher priority.

    The first argument denotes a function [plain] to represent a constructor with no formatting.
 *)

(** {1 Combinators} *)
val inside : (char * (char * bool)) list -> char -> 
  BatSubstring.t -> string option * BatSubstring.t
(** [inside table delim rest] uses [table] as its delimiter table to
    fetch the closing delimiter of [delim] and returns [Some string_in_between, rest]
    if such a closing occurence is found or [None, rest] otherwise *)

val inside_force : (char * (char * bool)) list -> char -> 
  BatSubstring.t -> string * BatSubstring.t
(** [inside_force] behaves similarly to {!inside} but instead of returning [None], it throws an exception *)

val see : string -> BatSubstring.t -> BatSubstring.t
(** [see prefix rest] returns [rest] without the prefix [prefix].
    If [rest] doesn't start by [prefix] then it throws an exception *)

val skip : ?n:int -> BatSubstring.t -> BatSubstring.t
(** [skip ~n s] is an alias for [triml] *)

val until : (char -> bool) -> BatSubstring.t -> string * BatSubstring.t
(** [until pred rest] returns the string with the first characters of rest such
    that [pred] is false on them, and the remaining substring starting to the
    first character on which [pred] returned true *)

val one_of : char list -> char -> bool
(** [one_of l c] is [List.mem c l] *)

val ( ||| ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [f ||| g] is the pointwise disjunction of [f] and [g] *)

val until_space : (char -> bool) -> BatSubstring.t -> string * BatSubstring.t
(** Same as {!until} but it may stop on space as well. *)
