(* Dynamic loading of plugins *)
open Prelude
open Batteries
open Plugin
open Config

module P = struct
  type interface = unit
  let data = ()
  let config = Config.create ()
  let name = "dynamic"
  let ocamlc = Config.add config "ocamlc" string "ocamlc command to use" 
    "ocamlfind ocamlc -c -package batteries,mlorg"
  let ocamlopt = Config.add config "ocamlopt" string "ocamlopt command to use" 
    "ocamlfind ocamlopt -shared -package batteries,mlorg"
end
open P
let () = Plugin.General.add (module P : Plugin with type interface = unit)

module Make (T : sig val name : string type t end) = struct
  let ref = ref None
  let register (t: T.t) = ref := Some t
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
let _ = %s.register (
# %d %S 1
%s
)"
      T.name number file (String.concat "\n" lines);
      close_out fd

  let is_newer f1 f2 = 
    let open Unix in
    try
      let s2 = stat f2 in
      try
        let s1 = stat f1 in
        s1.st_mtime >= s2.st_mtime
      with _ -> false
    with _ -> true

  let link obj = 
    try ref := None; Dynlink.loadfile obj; !ref
    with Dynlink.Error e ->
      Log.error "Error while loading %s: %s" obj (Dynlink.error_message e);
      None


  let compile ?(clean = true) ~file conf = 
    let obj = change_ext (if Dynlink.is_native then "cmx" else "cmo") file in
    let command = Printf.sprintf "%s %s -o %s"
      (if Dynlink.is_native then Config.get conf ocamlopt 
       else Config.get conf ocamlc)
      file
      obj
    in
    let () = 
      try
        Log.info "Compiling %s [%s]" file command;
        if is_newer file obj then Command.run command |> ignore
      with Command.Failed (command, error) ->
        Log.error "While compiling (command: %s):\n%s" command error;
    in
    let rm f = try Sys.remove f with _ -> () in
    let v = link obj in
    if clean then List.iter rm [file; obj; change_ext "cmi" obj; change_ext "o" obj];
    v

      
    let load ?ml_source conf lines file number = 
      match ml_source with
        | None ->
          File.with_temporary_out ~suffix:".ml" (fun fd source ->
            write_ml_source fd lines file number;
            compile conf ~file: source)
        | Some source ->
          File.with_file_out source (fun fd -> write_ml_source fd lines file number);
          compile ~clean: false conf ~file: source

    let load_source config source doc = 
      let ret ?ml_source file lines number = 
        load ?ml_source config lines file number in
      let file source = compile ~clean: false config ~file: source in
      let block ml_source = function
        | Block.Src {Block.linenumber; lines} -> 
          ret ?ml_source doc.Document.filename (List.map fst lines) linenumber
        | _ -> Log.warning "Denoted block is not a source block."; None
      in 
      let lines ml_source code = ret ?ml_source "<command-line>" 
        (String.nsplit ~by: "\n" code) 1 in
      try 
        let (Some b) = Document.find_block_by_name source doc in
        block None b
      with _ ->
        if Sys.file_exists source then file source
        else
          try Scanf.sscanf source "%[^:]:%[^:]:%[^\\00]" (fun typ ml_source data ->
            let ml_source = if ml_source = "" then None else Some ml_source in
            (match typ with
            | "block" -> 
              (try let (Some b) = Document.find_block_by_name data doc in
                   block ml_source b
               with _ -> Log.error "Invalid block named %s" data; None)
            | "ml" -> lines ml_source data))
          with _ -> lines None source
end
module U = Make (struct let name = "Dynamic" type t = unit end)
include U
