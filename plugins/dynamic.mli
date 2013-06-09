(** Dynamic loading of modules *)

(** This module defines a way for loading dynamic module register values of
    certain type *)

module Make (F : sig 
  val name : string 
  (** Named of the module to register on (should be the name of the
      module as it is exported) *)
  type t 
  (** Type of values to be registered *)
end) : sig
  val register : F.t -> unit
  (** register a value. To be used only by the generated source code *)

  val link : string -> F.t option
  (** [link binary_file] dynamically links the file [binary_file],
      waits for it to register a value and return it if it did, or None
      if something went wrong. *)

  val compile : ?clean : bool -> file: string -> Config.instance -> F.t option
  (** [compile ?clean ~file config] compiles ML file [files], {!link}
      it and returns the value. (The config is needed to know the path to ocaml)

      clean is used to know whether we should remove [file] and the
      generated files. NB: the name of the generated object file is
      deduced from the filename. *)

  val load : ?ml_source : string -> Config.instance -> string list -> string -> int -> F.t option
(** [load config lines filename linenumber] compiles the source code
    [lines] coming from [filename] and starting at [linenumber], and
    loads it, returing the registered value. The optional parameter if
    set may be used not to use a temporary file (It is then the path
    to the ML file the source code should be written in *)

  val load_source : Config.instance -> string -> Document.t -> F.t option
  (** [load_source config source doc] parses and compile a source code whose form can be:
      - [block:name]: then it refers to the content of the block [name] in the document
      - [file:file]: refers the content of file
      - [inline:source:code]: refers to [code] and specifies that it is to be written in [source] (optional) *)
      

end
(** This module given a name and a type will return a generic loader of dynamic
    module.  The name should be the name of the Make(...) module so that
    [name.register] points to the returned register function (used in the source
    during the compilation)
*)

(** {2 Effectful plugins} *)
(** Below is the signature of [Make(struct let name = "Dynamic" type t
    = unit)]. You can use it whenever you need the user to be able to
    register unit values *)

val register : unit -> unit
(** Register a unit value *)
val load : ?ml_source : string -> Config.instance -> string list -> string -> int -> unit option
(** Load a unit value *)
