(* Quote.ml: Implements an exporter
   that just calls the file to know how to export it ! *)
open Prelude
open Batteries
open Config
open Document
open Modules
let r = ref (fun _ _ -> print_endline "foo")
let register f = r := f
module E = struct
  module Meta = struct
    let name = "quote"
    let config = Config.create ()
    let ocamlc = Config.add config "ocamlc" string "OCamlc command to use" 
      (Printf.sprintf "ocamlfind %s -package batteries,mlorg"
         (if Dynlink.is_native then "ocamlopt -shared" else "ocamlc -c"))
    let block = Config.add config "block" string "Name of the codeblock to use to export" "export"
    let external_file = Config.add config "external-file" string "Optional name of the file to load" ""
    let code = Config.add config "code" string "Optional code (as a string) to load" ""
    let config = Config.validate config
  end
    
  let write_ml_source fd lines file number = 
    Printf.fprintf fd
"open Mlorg
open Batteries
open Printf
open Block
open Inline
module D = Document
module F = Filter
let write = Printf.fprintf
let _ = Backends.Quote.register (
# %d %S 1
%s
)"
      number file (String.concat "\n" lines);
      close_out fd

  let run ocamlc input lines number document out = 
    let fd, filename = File.open_temporary_out ~suffix:".ml" () in
    let () = write_ml_source fd lines input number in
    let obj = change_ext (if Dynlink.is_native then "cmx" else "cmo") filename in
    let command = Printf.sprintf "%s %s -o %s" ocamlc (Filename.quote filename)
      (Filename.quote obj) 
    in 
    let () = Log.info "Compiling %s [%s]" filename command;
      if Sys.command command <> 0 then
        Log.fatal "Compiling failed." 
    in
    let () = 
      try Dynlink.loadfile obj
      with Dynlink.Error e ->
        Log.fatal "Error while loading %s: %s" obj (Dynlink.error_message e)
    in
    !r document stdout

  let export { get } doc out = 
    if get Meta.external_file <> "" then
      run (get Meta.ocamlc) (get Meta.external_file)
        (File.lines_of (get Meta.external_file) |> List.of_enum) 1 doc out
    else if get Meta.code <> "" then
      run (get Meta.ocamlc) "<user-entry>"
        [get Meta.code] 1 doc out
    else
      match Document.find_block_by_name doc (get Meta.block) with
        | None -> Log.fatal "Block %s not found." (get Meta.block)
        | Some (Block.Src (number, _, lines)) ->
            run (get Meta.ocamlc) doc.filename lines number doc out
        | _ -> Log.fatal "Block %s has wrong type" (get Meta.block)
          
  let default_filename _ = "-"
end
let _ = Modules.Exporters.add (module E : Exporters.Signature)        
