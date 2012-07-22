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


let write_tex_source conf formula tex = 
  File.with_file_out tex (fun fd ->
    IO.nwrite fd "\\documentclass{article}\n\\pagestyle{empty}\n\n";
    IO.nwrite fd (conf.get header ^ "\n\\usepackage[active,tightpage,textmath,displaymath]{preview}\n\\begin{document}\n");
    IO.nwrite fd formula;
    IO.nwrite fd "\n\\end{document}\n"
  )

let compile { get } texfile = 
  Sys.command (Printf.sprintf "%s %s > /dev/null 2> /dev/null" (get latex) (Filename.quote texfile))

let dvi2png { get } texfile =
  Sys.command (Printf.sprintf "%s %s -o %s > /dev/null 2> /dev/null" (get dvipng) (Filename.quote (change_ext "dvi" texfile))
                 (Filename.quote (change_ext "png" texfile)))
  

let math2png { get } formula = 
  let hash = Digest.to_hex (Digest.string formula) in
  let pwd = Sys.getcwd () in
  let () = Sys.chdir (get dir) in
  let pngfile = hash ^ ".png" in
  let texfile = hash ^ ".tex" in
  let out = Filename.concat (get dir) pngfile in
  let rm ext = try Sys.remove (hash ^ "."^ext) with _ -> () in
  if not (Sys.file_exists pngfile) then
    (write_tex_source { get } formula texfile;
     compile { get } texfile;
     dvi2png { get } texfile; ();
     List.iter rm ["dvi"; "log"; "aux"; "tex"]
    );
  Sys.chdir pwd;
  out
    

let transform ({ get } as conf) doc = 
  let compute_total () = 
    let o  = object(self)
      inherit [int] Document.folder as super
      method inline k = function
        | Inline.Latex_Fragment (Inline.Math s) when get pinline ->
            k+1
        | x -> super#inline k x
      method block k = function
        | Math _ | Latex_Environment _ -> k+1
        | x -> super#block k x
    end in o#document 0 doc
  in
  let total = if get Plugin.Global.verbose >= 1 then
      compute_total ()
    else 0
  in
  let counter = ref 0 in
  let o = object(self)
    inherit [unit] Document.mapper as super
    method doit formula =
      let file = math2png conf formula in
      incr counter; self#message();
      Link {url=File file; label = [Plain formula]}
    method inline k = function
      | Inline.Latex_Fragment (Inline.Math s) when get pinline ->
          let source = "$" ^ s ^ "$" in
          self#doit source
      | x -> super#inline k x
    method block k = function
      | Math b when get pblock ->
          Custom ("center", "", [Paragraph [self#doit ("$$"^b^"$$")]])
      | Latex_Environment (name, opts, lines) when get pblock ->
          let source = Printf.sprintf "\\begin{%s}%s\n%s\n\\end{%s}"
            name opts (String.concat "\n" lines) name
          in
          Custom ("center", "", [Paragraph [self#doit source]])
      | x -> super#block k x
    method message () = 
      if get Plugin.Global.verbose >= 1 then
        (Printf.eprintf "\rProcessing images: %d%%"
          ((!counter * 100) / total);
         flush stderr)
  end
  in
  let doc' = o#document () doc in
  Printf.eprintf "\n"; doc'
