(** Modules. Defines the different kind of modules *)

module type Meta = sig
  val name : string
  val config : Config.t
end
(** The metadata of a module *)
type meta = (module Meta)

module Exporters : sig
  module type Signature = sig
    module Meta : Meta
    (** The exporter's metadata *)

    val default_filename : string -> string
    (** From the source, builds the default filename *)
    val export : Document.t -> unit BatIO.output -> unit
    (** The exporter function *)
  end
  (** A module that defines an exporters *)
  type t = (module Signature)
  (** The type of exporters *)
  val meta : t -> meta
  (** Returns the metadata of an exporter *)
  val find : string -> t
  (** Returns the exporter of the given name *)
  val run : string -> Document.t -> unit BatIO.output -> unit
  (** Run a given exporter *)

  val add : t -> unit
  (** Add an exporter *)
end
