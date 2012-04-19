(** Some utilities function *)

(** {2 String splitting} *)

val words : string -> string list
(** [words s] returns the list of words (that is space-separated chunks of
    character) in [s] *)

val lines : string -> string list
(** [lines s] returns the list of lines appearing in [s] *)


(** {2 Escapes management} *)

(** This part deals with escapes. Operations are defined on substrings for more
    convenience. *)

val is_escaped : BatSubstring.t -> int -> bool
(** [is_escaped s k] returns true iff s.[k] is escaped, that is preceded by
    an odd number of backslashes *)

val is_escaping : BatSubstring.t -> int -> bool
(** [is_escaping s k] returns true iff s.[k+1] is defined and [is_escaped s
    (k+1)] holds *)

val unescape : BatSubstring.t -> string
(** Removes all the escapes appearing in a string. *)

val escape : string list -> string -> string
(** Escape the given character in a string *)

val substitute : (string -> string) -> string -> string
(** Substitute a string with variables in it (uses [Buffer.add_substitute]) *)
