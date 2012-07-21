(* mlorg's main *)

open Batteries
open Mlorg

(* pure part *)
let generate filename backend output config = 
  try
    let export = Plugin.Exporters.find backend in
    let module E = (val export : Plugin.Exporter) in
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
    Plugin.Exporters.run export config doc fdout;
    if output = "-" then IO.close_out fdout

  with Sys_error msg -> Printf.eprintf "%s\n" msg
    | Not_found -> Printf.eprintf "Backend `%s' does not exist.\n" backend
let output = ref "" and filename = ref "-"
let backend = ref "html"
let opts = ref []

let add_opt s = try
  let (a, b) = String.split s "=" in
  opts := (a, b) :: !opts
  with Not_found -> Log.warning "%s: invalid option (should be: var=value)" s

let _ = if not !Sys.interactive then (
  let open Arg in
      parse ["--filename", Set_string filename, "Filename to convert (default: stdin)";
             "--output", Set_string output, "Output file";
             "--set", String add_opt, "Set an option (of the form foo=bar)";
             "--backend", Set_string backend, "Output backend"]
      (fun _ -> ()) "mlorg";
      generate !filename !backend !output (Plugin.global_config !opts)
)
