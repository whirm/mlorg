(** Table of contents operation *)

(** This modules defines what a table of contents is
    and provides functions to extract it from a document *)

type t
(** The type of a table of contents *)

val gather : Document.t -> t
(** Get the toc of a document *)

val mem : string -> t -> bool
(** [mem name t] returns true if a heading with name [name] appears in [t] *)
