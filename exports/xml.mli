(** XML export *)

type t
(** The type of xml data, constructed with {!block}
    and {!data} *)

val escape_entities : string -> string
(** Escape the main entities in a string *)

val data : string -> t
(** Construct a leaf of the tree *)

val block : ?attr : (string * string) list -> ?indent : bool -> string -> t list -> t
(** Construct a block with given attribute name and children. The indent
    parameter specify if it is allowed to indent the contents of this block (default true) *)

val output : 'a Batteries.IO.output -> t -> unit
(** Output the tree *)
