(** Simple logging *)

(** Very simple logging to stderr *)

val warning : ('a, unit, string, string, string, unit) format6 -> 'a
val error   : ('a, unit, string, string, string, unit) format6 -> 'a
val info    : ('a, unit, string, string, string, unit) format6 -> 'a
val fatal   : ('a, unit, string, string, string, unit) format6 -> 'a
(** This functions exits after printing the message *)
