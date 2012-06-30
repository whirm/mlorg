(* Quote.ml: Implements an exporter
   that just calls the file to know how to export it ! *)
open Batteries
open Config
open Document
open Modules
let r = ref (fun _ _ -> ())
let register f = r := f
module E = struct
  module Meta = struct
    let name = "quote"
    let config = Config.create ()
    let ocamlc = Config.add config "ocamlc" string "OCamlc command to use" "ocamlfind ocamlc -package batteries,mlorg -c"
    let code = Config.add config "name" string "Name of the codeblock to use to export" "export"
  end
    
  let compile file = 
    let command = Printf.sprintf "%s %s" (Config.get Meta.ocamlc) (Filename.quote file) in 
    Log.info "Compiling %s [%s]" file command;
    if Sys.command command <> 0 then
      (Log.fatal "Compiling failed."; failwith "Compilation failed")
        
  let load_and_run file document out = 
    compile file;
    let obj = Prelude.change_ext (if Dynlink.is_native then "cmx" else "cmo") file in
    (try Dynlink.loadfile obj
    with Dynlink.Error e ->
      (Log.fatal "Error while loading %s: %s" obj (Dynlink.error_message e);
      failwith "Cannot load module"));
      Sys.remove file; Sys.remove obj;
      !r document out
  
  let create_ml_source document = 
    match Document.find_block_by_name document (Config.get Meta.code) with
      | None -> Log.fatal "Block %s not found." (Config.get Meta.code);
          failwith "Block not found"
      | Some (Block.Src (number, _, lines)) ->
          let fd, filename = File.open_temporary_out ~suffix:".ml" () in
          Printf.fprintf fd 
"open Batteries;;
open Printf;;
module D = Document
module F = Filter
let write s o = IO.nwrite o s
open Block
open Inline
let _ = Quote.register (
# 1 %S %d
%s
)"
            document.filename number (String.concat "\n" lines);
          close_out fd;
          filename
      | _ -> Log.fatal "Block %s has wrong type" (Config.get Meta.code);
          failwith "Block has wrong type"
          

    let export doc out = 
      load_and_run (create_ml_source doc) doc out
  let default_filename _ = "-"
end
let _ = Modules.Exporters.add (module E : Exporters.Signature)        
