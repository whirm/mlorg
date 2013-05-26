(* Quote.ml: Implements an exporter
   that just calls the file to know how to export it ! *)
open Prelude
open Batteries
open Config
open Document
open Plugin

module Dynamic = Dynamic.Make (struct 
  let name = "Backends.Quote.Dynamic" 
  type t = Document.t -> unit IO.output -> unit
end)

module E = struct
  let name = "quote"
  let config = Config.create ()
  let source = Config.add config "source" string "Source of the outputter" "fun _ _ -> ()"
  module D = struct
    let export config doc out = 
      print_endline (Config.get config source);
      match Dynamic.load_source config (Config.get config source) doc with
      | None -> Log.error "No outputter for this document could be loaded."
      | Some f -> f doc out
            
    let default_filename _ = "-"
  end
  type interface = exporter
  let data = (module D : Exporter)
end
let _ = Exporters.add (module E : Plugin with type interface = exporter)        
