open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Modules
open Config

module E = struct
  module Meta = struct
    let name = "latex"
    let config = Config.create ()
    let classname = 
      add config "classname" string "The LaTeX class name to use" "article"
    let header = add config "header" string
      "The LaTeX header. You can use the following variables in it:
- classname: the class name chosen for this document
- packages: the list of packages to be loaded (formatted)
- extraheader: user's extra header
- title, author: document's metadata"
"\\documentclass{$classname}
$packages
$extraheader
\\title{$title}
\\author{$author}
\\begin{document}
"
    let footer = add config "footer" string "The LaTeX footer"
"\\end{document}"
    let extraheader = add config "extraheader" string "Extra LaTeX header" ""
  end
  open Meta
  let assoc l s = try List.assoc s l with _ -> ""
  let export doc out = 
    let o = object(self)
      inherit [unit] Document.folder as super

      method escape_inside = escape ["}"]
      method escape = escape ["}"; "{"; "$"; "\\"; "["; "]"]
      method header () = 
        let vars = ["classname", self#escape_inside (get classname);
                    "packages", "";
                    "extraheader", Config.get extraheader;
                    "title", self#escape_inside doc.title;
                    "author", self#escape_inside doc.author
                   ] in
        IO.nwrite out (substitute (assoc vars) (get header))
      method footer () = 
        IO.nwrite out (get footer)
      method inline () = function
        | Plain s -> IO.nwrite out (self#escape s)
        | Emphasis (kind, data) ->
          let l = [`Bold, "textbf"; `Italic, "emph"; `Underline, "underline"] in
          IO.printf out "\\%s{" (List.assoc kind l);
          self#inline_list () data;
          IO.printf out "}"
        | Entity e -> 
          if not e.latex_mathp then 
            IO.printf out "%s" e.latex
          else
            IO.printf out "$%s$" e.latex
        | Latex_Fragment s ->
          IO.printf out "$%s$" (escape ["$"] s)
        | Verbatim s -> 
          IO.printf out "\\texttt{%s}" (self#escape s)
        | x -> super#inline () x
      method block () = function
        | Paragraph l -> self#inline_list () l; IO.printf out "\n\n"
        | x -> super#block () x
      method document () d = 
        self#header ();
        super#document () d;
        self#footer ()
    end
    in o#document () doc
end

let _ = Modules.Exporters.add (module E : Exporters.Signature)        
