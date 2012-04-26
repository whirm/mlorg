(* mlorg's main *)

open Batteries
open Prelude
open Html
open Latex

(* pure part *)
let generate filename backend output = 
  let export = Modules.Exporters.find backend in
  try
    let module E = (val export : Modules.Exporters.Signature) in
    let output = if output = "" then 
        E.default_filename filename
      else
        output
    in
    let fdout = if output = "-" then
        IO.stdout
      else
        File.open_out output
    in
    let doc = if filename = "-" then 
        Document.from_chan "<stdin>" stdin
      else 
        Document.from_file filename
    in
    Modules.Exporters.run backend doc fdout;
    if output = "-" then IO.close_out fdout

  with Sys_error msg -> Printf.eprintf "%s\n" msg
    | Not_found -> Printf.eprintf "Backend `%s' does not exist." backend
let output = ref "" and filename = ref "-"
let backend = ref "html"


let _ = 
  let open Arg in
      parse ["--filename", Set_string filename, "Filename to convert (default: stdin)";
             "--output", Set_string output, "Output file";
             "--backend", Set_string backend, "Output backend"]
      (fun _ -> ()) "mlorg";
      generate !filename !backend !output
