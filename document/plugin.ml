open Document
open Prelude
open Batteries

(* General plugin system *)
module type Plugin = sig
  val name : string
  val config : Config.t
    
  type interface 
  val data : interface
end


type _plugin = (module Plugin)
type 'a plugin = (module Plugin with type interface = 'a)

let find (type u) name = 
  List.find (fun m ->
    let module M = (val m : Plugin with type interface = u) in
    M.name = name)

let data (type u) (m : u plugin) = 
  let module M = (val m : Plugin with type interface = u) in
  M.data

let name (type u) (m : u plugin) = 
  let module M = (val m : Plugin with type interface = u) in
  M.name

let config (type u) (m : u plugin) = 
  let module M = (val m : Plugin with type interface = u) in
  M.config

(* Specific plugins type *)

(* Exporters *)
module type Exporter = sig
  val default_filename : string -> string
  val export : Config.instance -> Document.t -> unit BatIO.output -> unit
end
type exporter = (module Exporter)

  
module Exporters = struct
  include ExtList.Make (struct
    type t = exporter plugin
    let base = []
  end)

  let run exporter instance doc output = 
    let module M = (val exporter : Exporter) in
    M.export instance doc output

  let find n = data (find n (get ()))
  let add = push
  let config () = Config.concat (List.map (fun e -> name e, config e) (get ()))
end

module General = struct
  include ExtList.Make (struct
    type t = unit plugin
    let base = []
  end)
  let add = push
  let config () = Config.concat (List.map (fun e -> name e, config e) (get ()))
end

module Global = struct
  open Config
  let config = Config.create ()
  let verbose = Config.add config "verbose" int "Verbosity level" 0
end
let get_global_interface () =
  Config.concat
    [
      "exporters", Exporters.config ();
      "general", General.config ();
      "mlorg", Global.config
    ]

let global_config parameters = 
  Config.make (get_global_interface ()) parameters
  
let eprint_config_descr = 
  get_global_interface %> Config.prettyprint stderr

let options_man = 
  get_global_interface %> Config.to_man
