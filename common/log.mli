(** Simple logging *)

(** This modules implements a very basic logging system
    that will print the messages to stderr. In the future this might be improved. *)


val warning : ('a, unit, string, string, string, unit) format6 -> 'a
val error   : ('a, unit, string, string, string, unit) format6 -> 'a
val info    : ('a, unit, string, string, string, unit) format6 -> 'a

val debug   : ('a, unit, string, string, string, unit) format6 -> 'a

val fatal   : ('a, unit, string, string, string, 'b) format6 -> 'a
(** This functions exits after printing the message *)
