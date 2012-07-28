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
      let ml_source = if config.get save_to = "" then None
        else Some (config.get save_to)
      in
      match Dynamic.load ?ml_source config lines file number with
        | Some f -> f doc out
        | None -> ()
    let export ({ get } as conf) doc out = 
      if get external_file <> "" then
        run conf 
          (File.lines_of (get external_file) |> List.of_enum) 
          (get external_file) 1 doc out
      else if get code <> "" then
        run conf [get code] "<user-entry>" 1 doc out
      else
        match Document.find_block_by_name doc (get block) with
          | None -> Log.fatal "Block %s not found." (get block)
          | Some (Block.Src (number, _, lines)) ->
            run conf lines doc.filename number doc out
          | _ -> Log.fatal "Block %s has wrong type" (get block)
            
    let default_filename _ = "-"
  end
  type interface = exporter
  let data = (module D : Exporter)
end
let _ = Exporters.add (module E : Plugin with type interface = exporter)        
