(* math2png *)
open Prelude
open Plugin
open Inline
open Block
open Config
open Batteries

module P = struct
  let name = "math2png"
  let config = Config.create ()
  let dir = Config.add config "dir" string "The directory where to store images of preprocessed formulas"
    "lxtpng"
  let latex = Config.add config "latex" string "The latex command to invoke" "latex -interaction=batchmode"
  let dvipng = Config.add config "dvipng" string "The dvipng command to invoke" "dvipng"
  let header = Config.add config "header" string "Header to add to the tex files" ""
  let pinline = Config.add config "inline" boolean "Tell whether inline math should be processed" false
  let pblock = Config.add config "inline" boolean "Tell whether block math (between $$) should be processed" true
  type interface = unit
  let data = ()
end
let () = Plugin.General.add (module P : Plugin with type interface = unit)
open P


let write_tex_source conf formulas tex = 
  File.with_file_out tex (fun fd ->
    IO.nwrite fd "\\documentclass{article}\n\\pagestyle{empty}\n\n";
    IO.nwrite fd (conf.get header ^ "\n\\usepackage[active,tightpage,textmath,displaymath]{preview}\n\\begin{document}\n");
    List.iter (Printf.fprintf fd "%s\n") formulas;
    IO.nwrite fd "\n\\end{document}\n"
  )

let compile { get } texfile = 
  Sys.command (Printf.sprintf "%s %s > /dev/null 2> /dev/null" (get latex) (Filename.quote texfile))

let parse_dvipng_output s = 
  let open BatSubstring in
  let int_of_substring s =
    let rec aux k s = 
      if length s = 0 then k, s
      else match get s 0 with
        | '0' .. '9' -> aux (10*k + int_of_char (get s 0) - int_of_char '0') (triml 1 s)
        | _ -> k, s
    in aux 0 s
  in
  let rec parse_out acc s = 
    try
      let k = BatSubstring.index s '=' in
      let s = BatSubstring.triml (1+k) s in
      let n, s = int_of_substring s in
      parse_out (n :: acc) s
    with _ -> List.rev acc
  in parse_out [] (of_string s)

let dvi2png { get } texfile =
  let command = 
    Printf.sprintf "%s %s --depth -o %%d%s 2> /dev/null" (get dvipng) 
      (Filename.quote (change_ext "dvi" texfile))
      (Filename.quote (change_ext "png" texfile))
  in
  let fdin = Unix.open_process_in ~autoclose: true ~cleanup: true command in
  let result = parse_dvipng_output (IO.read_all fdin) in
  let _ = Unix.close_process_in fdin in
  result
  

let math2png { get } formulas = 
  let list = List.map 
    (fun s -> s, Filename.concat (get dir)
      (Digest.to_hex (Digest.string s) ^ ".png")) 
    formulas 
  in
  let pwd = Sys.getcwd () in
  let () = Sys.chdir (get dir) in
  let texfile = "_tmp.tex" in
  let rm ext = try Sys.remove ("_tmp."^ext) with _ -> () in
  if list = [] then []
  else begin
    let _ = 
      Log.info "Processing images (%d)..." (List.length list);
      write_tex_source { get } formulas texfile;
      compile { get } texfile
    in
    let depths = dvi2png { get } texfile in
    let () = 
      if List.length list <> List.length depths then
        (Log.fatal "Error while generating images at fragment:\n %s"
           (fst (List.at list (List.length depths))))
    in
    let list = List.map2 (fun (a, b) c -> (a, (b, c))) list depths in
    let () = 
      List.iter rm ["dvi"; "log"; "aux"; "tex"];
      Sys.chdir pwd;
      List.iteri (fun k (a, (b, c)) -> 
        Sys.rename (Printf.sprintf "%s/%d_tmp.png" (get dir) (1+k)) b) list
    in list
  end

let transform ({ get } as conf) doc = 
  let gather_formulas () = 
    let o  = object(self)
      inherit [string list] Document.folder as super
      method inline l = function
        | Inline.Latex_Fragment (Inline.Math s) when get pinline ->
            ("$" ^ s ^ "$") :: l
        | x -> super#inline l x
      method block l = function
        | Math b when get pblock ->
            ("$$" ^ b ^ "$$") :: l
        | Latex_Environment (name, opts, lines) when get pblock ->
          let source = Printf.sprintf "\\begin{%s}%s\n%s\n\\end{%s}"
            name opts (String.concat "\n" lines) name
          in
          source :: l
        | x -> super#block l x
    end in o#document [] doc
  in
  let formulas = math2png conf (gather_formulas ()) in
  let o = object(self)
    inherit [unit] Document.mapper as super
    method lookup formula =
      let (s, n) = List.assoc formula formulas in
      Link {url=Complex {protocol="depth-"^string_of_int n; link = s}; 
            label = [Plain formula]}
    method inline k = function
      | Inline.Latex_Fragment (Inline.Math s) when get pinline ->
          let source = "$" ^ s ^ "$" in
          self#lookup source
      | x -> super#inline k x
    method block k = function
      | Math b when get pblock ->
          Custom ("center", "", [Paragraph [self#lookup ("$$"^b^"$$")]])
      | Latex_Environment (name, opts, lines) when get pblock ->
          let source = Printf.sprintf "\\begin{%s}%s\n%s\n\\end{%s}"
            name opts (String.concat "\n" lines) name
          in
          Custom ("center", "", [Paragraph [self#lookup source]])
      | x -> super#block k x
  end
  in
  let doc' = o#document () doc in
  Printf.eprintf "\n"; doc'
