(** Delimiters management *)

(** To parse configurations or Inline contents, delimiters are widely used. The
    idea is to specify a table (delimiters table) and get functions to get a
    string enclosed by a given delimiter, to split among a delimiter. *)

(** Note that the delimiter parsing is more subtle than what you can get with
    for instance [Str.split]. In particular it will match other delimiters and
    handle escpaes. For instance with the proper table, [enclosing_delimiter
    "{foo{bar}}" '{'] would return ["foo{bar}"]. Not all delimiters are
    considered as 'valid'. Non-valid delimiters are ignored. A delimiter is valid when:

    - it is not escaped
    - it is not surrounded by space
*)

module type Table = sig
  val table : (char * (char * bool)) list
end
(** A table, composed of elements of the form [(opening, (closing, quote))].
    If [quote] is true it means that the contents held inside this delimiter is to be quoted
    and you don't have to look for delimiters inside.
*)
module Make (T : Table) : sig
  val enclosing_delimiter : ?valid: bool -> BatSubstring.t -> char -> (string * BatSubstring.t) option
  (** [enclosing_delimiter s c] expects a substring starting by [c]. It will
      search for the end of the delimited string. On success it will return the
      delimited string along with rest of the substring following the closing
      delimiter. Otherwise it will return [None]. [valid] is used to tell if you require that the closing delimiter should be valid. *)

  val split : BatSubstring.t -> char -> string list
  (** [split sub c] will split [sub] along the closing delimiter corresponding
      to [c] *)

  val closing_delimiter : char -> char option
  (** Given a char [c] returns the corresponding closing delimiter
      corresponding to [c] if it exists *)
  val starting_delimiter : char -> char option
  (** Given a char [c] returns the corresponding closing delimiter
    corresponding to [c] if it exists *)
end
