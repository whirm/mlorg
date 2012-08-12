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
  let block = Config.add config "block" string "Name of the codeblock to use to export" "export"
  let external_file = Config.add config "external-file" string "Optional name of the file to load" ""
  let code = Config.add config "code" string "Optional code (as a string) to load" ""
  let save_to = Config.add config "save" string "Non-empty: set to a ML file in which the generated source will be put (instead of a temporary file" ""
  module D = struct
    let run config lines file number doc out = 
      let ml_source = if Config.get config save_to = "" then None
        else Some (Config.get config save_to)
      in
      match Dynamic.load ?ml_source config lines file number with
        | Some f -> f doc out
        | None -> ()
    let export config doc out = 
      if Config.get config external_file <> "" then
        run config 
          (File.lines_of (Config.get config external_file) |> List.of_enum)
          (Config.get config external_file) 1 doc out
      else if Config.get config code <> "" then
        run config [Config.get config code] "<user-entry>" 1 doc out
      else
        match Document.find_block_by_name doc (Config.get config block) with
          | None -> 
            Log.fatal "Block %s not found." (Config.get config block)
          | Some (Block.Src (number, _, lines)) ->
            run config lines doc.filename number doc out
          | _ -> 
            Log.fatal "Block %s has wrong type" (Config.get config block)
            
    let default_filename _ = "-"
  end
  type interface = exporter
  let data = (module D : Exporter)
end
let _ = Exporters.add (module E : Plugin with type interface = exporter)        
