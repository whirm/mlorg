(** Dynamic loading of modules *)

(** This module defines a way for loading dynamic module register values of
    certain type *)

module Make (F : sig val name : string type t  end) : sig
  val register : F.t -> unit
  (** register a value. To be used only by the generated source code *)
  val load : ?ml_source : string -> Config.instance -> string list -> string -> int -> F.t option
(** [load config lines filename linenumber] compiles the source code [lines]
    coming from [filename] and starting at [linenumber], and loads it, returing the
    registered value. The optional parameter if set may be used not to use a
    temporary file (It is then the path to the ML file the source code should be written in *)
end
(** This modules given a name and a type will return a generic loader of dynamic
    module.  The name should be the name of the Make(...) module so that
    [name.register] points to the returned register function (used in the source
    during the compilation)
*)
val register : unit -> unit
val load : ?ml_source : string -> Config.instance -> string list -> string -> int -> unit option
