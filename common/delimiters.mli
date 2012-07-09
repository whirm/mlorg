(** Delimiters management *)

(** To parse configurations or Inline contents, delimiters are widely used. The
    idea is to specify a table (delimiters table) and get functions to get a
    string enclosed by a given delimiter, to split among a delimiter. *)

(** Note that the delimiter parsing is more subtle than what you can get with
    for instance [Str.split]. In particular it will match other delimiters and
    handle escpaes. For instance with the proper table, [enclosing_delimiter
    "{foo{bar}}" '{'] would return ["foo{bar}"].
*)

module type Table = sig
  val table : (char * (char * bool)) list
end
(** A table, composed of elements of the form [(opening, closing, in_word?)]
    where the boolean [in_word?] tell whether an occurence of either opening or
    closing delimiter inside a word is considered valid.
*)
module Make (T : Table) : sig
  val enclosing_delimiter : BatSubstring.t -> char -> (string * BatSubstring.t) option
  (** [enclosing_delimiter s c] expects a substring starting by [c]. It will
      search for the end of the delimited string. On success it will return the
      delimited string along with rest of the substring following the closing
      delimiter. Otherwise it will return [None] *)

  val split : BatSubstring.t -> char -> string list
  (** [split sub c] will split [sub] along the closing delimiter corresponding
      to [c] *)
end
