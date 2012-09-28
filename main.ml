(* mlorg's main *)

open Batteries
open Mlorg
open Document

(* pure part *)
let generate filename backend output opts = 
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
    let config = Plugin.global_config (opts @ doc.opts) in
    Plugin.Exporters.run export config doc fdout;
    if output = "-" then IO.close_out fdout

  with Sys_error msg -> Printf.eprintf "%s\n" msg
    | Not_found -> Printf.eprintf "Backend `%s' does not exist.\n" backend
let output = ref "" and filename = ref "-"
let backend = ref "html"
let opts = ref []

let add_opt_i key value = opts := (key, value) :: !opts

let add_opt s = try
  let (a, b) = String.split s "=" in
  opts := (a, b) :: !opts
  with Not_found -> Log.warning "%s: invalid option (should be: var=value)" s

let with_filename s f = 
  if s = "-" then f stdin
  else BatFile.with_file_in s f
let get_directive s = 
  with_filename !filename
      (fun fd -> try
        Org_parser.parse_lazy (BatIO.lines_of fd) |>
            Enum.find_map (function
              | Block.Directive (key, value) when key = s -> Some value
              | _ -> None) |> print_endline |> fun () -> exit 0
        with Not_found -> Log.fatal "Directive %s not found." s)

let _ = if not !Sys.interactive then (
  let open Arg in
      parse ["--filename", Set_string filename, "Filename to convert (default: stdin)";
             "--output", Set_string output, "Output file";
             "--set", String add_opt, "Set an option (of the form foo=bar)";
             "--directive", String get_directive, "Output the contents of the given directive";
             "--custom", String (fun s ->
               backend := "quote";
               if Sys.file_exists s then
                 add_opt_i "exporters.quote.external-file" s
               else
                 add_opt_i "exporters.quote.block" s), "<name> Run a custom exporter on the file (using Quote). <name> can be either a file (containing ML source code) or the name of a source block inside the document";
             "--possible-options", Unit (fun () -> Plugin.eprint_config_descr (); exit 0), "Describe possible options of backends";
              "--backend", Set_string backend, "Output backend"]
      (fun s -> filename := s) "mlorg";
      generate !filename !backend !output !opts
)
