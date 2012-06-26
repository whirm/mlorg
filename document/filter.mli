(** Filters *)

(** This file defines basic filter on headings *)


type t = Document.heading -> bool
(** The type of a filter *)

type string_matcher = string -> bool
(** The type of a string matcher: a function matching strings *)

val s: string -> string_matcher
(** [s "foo"] matches only ["foo"] *)

val r: Str.regexp -> string_matcher
(** [r regexp] matches only what [regexp matches] *)

val rs: string -> string_matcher
(** [rs "regexp"] matches only what [Str.regexp "regexp"] matches *)

val has_property : string_matcher -> t
(** Selects only headings with the given property *)

val prop_val : string -> string_matcher -> t
(** [prop_val name v] selects headings which have the property [val] matching [v] *)

val has_tag : string_matcher -> t
(** Selects only headings with the given tag *)

val name : string_matcher -> t
(** Selects headings named string *)


val under : t -> t
(** [under f] selects headings that are children of a node matched by [f] *)

(** Combinators *)
val ( &&& ) : t -> t -> t
val ( ||| ) : t -> t -> t

val run : t -> Document.t -> Document.heading list
(** Run the filter on a document *)

val run_headings : t -> (Document.heading list -> Document.heading list)
(** Run on a list of headings *)

val run_headings_sub : t -> (Document.heading list -> Document.heading list)
(** Run on a list of headings and their children *)

