(** Handling fancy numbering *)

(** This module is used in numbered lists and headings to provide with fancy
    numbering. The idea is that the user specify a format for the number such as
    [I.i.α]. Then you take this format, and a list of numbers [[2; 3; 4]],
    and it'll give you the string [II.iii.δ].

    This module can also recover number from a string: on [II.iii.δ] it may retrieve [[2; 3; 4]]
 *)

(** {1 Numbering system} *)
type system = { encode : int -> string;
                decode : string -> int option * string }
(** A numbering system is just a way to (de)encode a number.  To decode, it
    should work only on partial strings and thus should return the non-used
    characters *)

(** {2 Ways to define systems} *)

(** We provide here two main ways to define systems : alphabetical and romain *)

val alphabetic_sys : string array -> system
(** Creates an alphabetic system. For instance [alphabetic_sys ["0"; ..; "9"]]
    creates the decimal system *)

val romain_sys : string array -> system
(** Creates a romain system. For instance [romain_sys ["I"; "V"; "X"; "L"; "C"; "D"; "M"]]
    creates the usual romain numbering. *)

module Systems : ExtList.ExtList with type elt = system
(** The list of numbering systems. Contains decimal system, romain both
    uppercase and lowercase, alphabetic (latin and greek) systems, both
    uppercase and lowercase. *)


val extract : string -> int list
(** Extracts the number appearing in a string.
    For instance [extract "I.i"] returns [[1; 1]] *)

val update : ?trunc : bool -> string -> int list -> string
(** Updates a format. For instance [update "I.i" [2; 2]] returns ["II.ii"].  If
    [trunc] is set to true (default: false) then the string is truncate when all
    the numbers in the list have been consumed.
*)
