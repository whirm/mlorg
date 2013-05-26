(* mlorg's main *)

open Batteries
open Mlorg
open Document
open Cmdliner
(* pure part *)
let with_filename s f = 
  if s = "-" then f stdin
  else BatFile.with_file_in s f

let with_document opts source f =
  let config = Plugin.global_config opts in
  if source = "-" then
    f (Document.from_chan config "<stdin>" stdin)
  else
    f (Document.from_file config source)

let generate backend output opts filename = 
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
    with_document opts filename (fun (doc, config) ->
      Plugin.Exporters.run export config doc fdout;
      if output <> "-" then IO.close_out fdout)
  with Sys_error msg -> Printf.eprintf "%s\n" msg
    | Not_found -> Printf.eprintf "Backend `%s' does not exist.\n" backend

(* Cmd liner part *)

(* Commonon options *)
let output = 
  let doc = "Write the generated file to $(docv). " in
  Arg.(value & opt string "" & info ["o"; "output"] ~docv:"OUTPUT-FILE" ~doc)

let backend = 
  let doc = "Uses $(docv) to generate the output. (`-` for stdout)" in
  Arg.(value & opt string "backend" & info ["b"; "backend"] ~docv:"BACKEND" ~doc)

let filename = 
  let doc = "The input filename to use. (`-` for stdin) " in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILENAME" ~doc)

let options = 
  let doc = "Extra option to use to configure the behaviour. (Can be used multiple times)" in
  Arg.(value & opt_all (pair ~sep:'=' string string) [] & info ["o"; "option"] ~docv: "OPTIONS" ~doc)

(*let get_directive s = 
  with_filename !filename
      (fun fd -> try
        Org_parser.parse_lazy (BatIO.lines_of fd) |>
            Enum.find_map (function
              | Block.Directive (key, value) when key = s -> Some value
              | _ -> None) |> print_endline |> fun () -> exit 0
        with Not_found -> Log.fatal "Directive %s not found." s)
*)

let cmd = 
  Term.(pure (fun a b c -> List.map (generate a b c)) 
          $ backend $ output $ options $ filenames)

let doc = "converts org-mode files into various formats"
let options = [
  `S "PLUGINS OPTIONS";
  `P "Here are the options recognized by the various modules inside mlorg."
] @ Plugin.options_man ()
let man = [
  `S "DESCRIPTION";
  `P "$(tname) converts org-mode files into other formats such as LaTeX, HTML, XML, and so on.";
] @ options

let infos = Term.info "mlorg" ~version:"0" ~doc ~man
let _ = if not !Sys.interactive then (
  match Term.eval (cmd, infos) with `Error _ -> exit 1 | _ -> exit 0
)

(*  let open Arg in
      parse ["--filename", Set_string filename, "Filename to convert (default: stdin)";
             "--output", Set_string output, "Output file";
             "--set", String add_opt, "Set an option (of the form foo=bar)";
             (*             "--directive", String get_directive, "Output the contents of the given directive";*)
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
*)
