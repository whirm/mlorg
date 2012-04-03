(** Handling time *)

(** This file deals with timestamps in org-mode. Timestamps are ubiquitous in
    org-mode files. *)

(** {1 Definitions} *)
type date = {
  year: int;
  month: int;
  day: int;
}
(** A date. *)

type time = {
  hour: int;
  min: int;
}
(** A time. No seconds in org-mode's timestamps *)
type t = {
  date: date;
  time: time option;
  repetition: date option;
(** The date to wait for the event to start again, eg. [+1w] corresponds to
    seven days *)
}
(** A timestamp. time and repetition are optional. *)

type range = {
  start: t;
  stop: t;
}
(** A timestamp range *)

(** A few accessors: *)

val year : t -> int
val month : t -> int
val day : t -> int

val hour : t -> int
val min : t -> int

val hour_opt : t -> int option
val min_opt : t -> int option

(** The null timestamp *)
val null : t

(** {1 Converting to and from {!Unix.tm}} *)

val to_tm : t -> Unix.tm
(** Converts a timestamp into an {!Unix.tm} *)

val from_tm : Unix.tm -> t
(** Converts a {!Unix.tm} to a timestamp, by normalizing it before.
    This is always set a time and no repetition *)

val normalize : t -> t
(** Normalize a timestamp. Note that this is not the composition of {!to_tm} and
    {!from_tm} as it preserves repetition and absence/presence of time *)

val weekday : t -> int
(** Returns the weekday of a timestamp *)

(** {1 Reading and writing timestamps} *)
val parse : string -> (t * string) option
(** Parse a timestamp *)

val parse_range : string -> (range * string) option
(** Parse a timestamp range *)

val parse_substring : BatSubstring.t -> (t * BatSubstring.t) option
(** Parse a substring as a timestamp *)

val parse_range_substring : BatSubstring.t -> (range * BatSubstring.t) option
(** Parse a substring as a range *)

val to_string : ?wday: string array -> t -> string
(** Converts a timestamp to a string.
    The optional array wday specifies the name of the weekday
*)

val range_to_string : range -> string
(** Converts a range to a string *)
