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

val unescape : ?l : char list -> BatSubstring.t -> string
(** [unescale ~l substring] unescapes characters in [l] appearing in [substring].
    If [l] is not specified then every escaped character is unescaped *)


val escape : string list -> string -> string
(** [escape chars string] escapes every occurence of any character in [chars], in [string] *)

val substitute : (string -> string) -> string -> string
(** [substitute f s] substitues every variable present in [s] by applying [f] on the name of the variable. It uses the same format as [Buffer.add_substitute] *)

(** {2 Other} *)

val change_ext : string -> string ->string
(** [change_ext new_ext file] replaces the old extension of [file] by [new_ext].
    Does nothing if [file = ""]. *)
