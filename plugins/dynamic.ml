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

  let load obj = 
    try Dynlink.loadfile obj
    with Dynlink.Error e ->
      Log.fatal "Error while loading %s: %s" obj (Dynlink.error_message e)
  let is_newer f1 f2 = 
    let open Unix in
    try
      let s2 = stat f2 in
      try
        let s1 = stat f1 in
        s1.st_mtime >= s2.st_mtime
      with _ -> false
    with _ -> true

  let compile ?(clean = true) { get } source obj = 
    let command = Printf.sprintf "%s %s -o %s"
      (if Dynlink.is_native then get ocamlopt else get ocamlc)
      source
      obj
    in
    let () = 
      Log.info "Compiling %s [%s]" source command;
      if Sys.command command <> 0 then
        Log.fatal "Compiling failed." 
    in
    let () = load obj in
    if clean then
      (Sys.remove source; Sys.remove obj;
       Sys.remove (change_ext "cmi" obj))
        

    let load ?ml_source conf lines file number = 
      match ml_source with
        | None ->
          File.with_temporary_out ~suffix:".ml" (fun fd source ->
            let obj = change_ext (if Dynlink.is_native then "cmx" else "cmo") source in
            write_ml_source fd lines file number;
            compile conf source obj;
            !ref)
        | Some source ->
          let obj = change_ext (if Dynlink.is_native then "cmx" else "cmo") source in
          if is_newer source obj then
            File.with_file_out source (fun fd ->
              write_ml_source fd lines file number;
              compile ~clean: false conf source obj
            )
          else
              load obj;
          !ref
end
module U = Make (struct let name = "Dynamic" type t = unit end)
include U
