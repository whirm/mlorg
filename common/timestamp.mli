(** Handling time *)

(** This file deals with timestamps in org-mode. Timestamps are ubiquitous in
    org-mode files. *)

(** {3 Definitions} *)

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
  active : bool;
(** Is the timestamp active ? [<timestamp>] is active and shows up in the agenda where [[timestamp]] isn't. *)
}
(** A timestamp. time and repetition are optional. *)

type range = {
  start: t;
  stop: t;
}
(** A timestamp range: a beginning and an end *)

(** {3 A few accessors} *)

val year : t -> int
val month : t -> int
val day : t -> int

val hour : t -> int
val min : t -> int

val hour_opt : t -> int option
val min_opt : t -> int option

val null : t
(** The null timestamp *)

(** {3 Converting to and from {!Unix.tm}} *)

val to_tm : t -> Unix.tm
(** Converts a timestamp into an {!Unix.tm} *)

val from_tm : ?active : bool -> Unix.tm -> t
(** Converts a {!Unix.tm} to a timestamp, by normalizing it before.
    This is always set a time and no repetition *)

val normalize : t -> t
(** Normalize a timestamp. Note that this is not the composition of {!to_tm} and
    {!from_tm} as it preserves repetition and absence/presence of time *)

val weekday : t -> int
(** Returns the weekday of a timestamp *)

(** {3 Reading and writing timestamps} *)

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

(** {3 Durations} *)

val seconds_of_t : t -> float
(* Returns the number of seconds denoted by a timestamp (since EPOCH) *)


val duration : range -> int
(** Returns the duration of a timestamp range, in seconds *)

val from_now : t -> int
(** [from_now t] return sthe number of seconds between now and [t] *)

val string_of_seconds : int -> string
(** Prettyprints a number of seconds *)

val add_days : t -> int -> t
(** Adds some days to a date *)

val today : unit -> t
(** Returns the timestamp corresponding to today *)

val covers : t -> t -> bool
(** [covers arg source] tells whether [arg] is covered by [source], meaning that

    - If [source] has no repetition marker then [is_covered_by source arg] is
    equivalent to [source.date = arg.date], ie. [source] and [date] happens on
    the same day
    - If [source has a repetition marker, then [is_covered_by source arg] is true
    whenever [arg] happens on a day where [source] repeats (including [source.date = arg.date])
*)
