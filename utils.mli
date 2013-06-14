(** Utils function for the library mlorg *)

val modify_file : ?config: Config.instance -> 
  (Document.t -> Document.t) -> string -> unit
(** [modify_file f filename] parses [filename], apply [f]
    on the corresponding document and writes back to [filename].

    You can optionally pass a custom configuration.
 *)

val get_meta : string -> (string * string) list
(** Returns the metadata of an org file (directives) *)
