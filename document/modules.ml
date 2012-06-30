open Document
open Prelude
open Batteries

module type Meta = sig
  val name : string
  val config : Config.t
end
(** The metadata of a module *)
type meta = (module Meta)

module Exporters = struct
  module type Signature = sig
    module Meta : Meta
    (** The exporter's metadata *)
      
    val default_filename : string -> string
    val export : Config.instance -> Document.t -> unit BatIO.output -> unit
    (** The exporter function *)

  end
  (** A module that defines an exporters *)
  type t = (module Signature)

  type tmp = t

  let meta v = 
    let module M = (val v : Signature) in
    (module M.Meta : Meta)

  module Exporters = ExtList.Make (struct
    type t = tmp
    let base = []
  end)
  let find name = 
    List.find (fun x ->
      let module M = (val meta x : Meta) in
      M.name = name)
      (Exporters.get ())

  let run exporter opts doc output = 
    let module M = (val find exporter : Signature) in
    let instance = Config.make M.Meta.config 
      (opts @ (try List.assoc exporter doc.ext_opts with _ -> [])) in
    M.export instance doc output
      
  let add = Exporters.push
end
