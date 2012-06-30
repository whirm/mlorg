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
\\maketitle
\\tableofcontents
"
    let footer = add config "footer" string "The LaTeX footer"
"\\end{document}"
    let extraheader = add config "extraheader" string "Extra LaTeX header" ""

    let sections = add config "sections" (list string)
      "The name of the sections"
      ["section"; "subsection"; "subsubsection"; "paragraph"; "subparagraph"]
  end
  open Meta
  let assoc l s = try List.assoc s l with _ -> ""
  let export doc out = 
    let o = object(self)
      inherit [unit] Document.folder as super

      method escape_inside s = s
      method escape = escape ["}"; "{"; "$"; "\\"; "["; "]"]
      method header () = 
        let vars = ["classname", self#escape_inside (get classname);
                    "packages", "";
                    "extraheader", Config.get extraheader;
                    "title", self#escape_inside doc.title;
                    "author", self#escape_inside doc.author;
                   ] in
        IO.nwrite out (substitute (assoc vars) (get header))
      method footer () = 
        IO.nwrite out (get footer)
      method inline () = function
        | Plain s -> IO.nwrite out (self#escape s)
        | Emphasis (kind, data) ->
          let l = [`Bold, "textbf"; `Italic, "emph"; `Underline, "underline"] in
          Printf.fprintf out "\\%s{" (List.assoc kind l);
          self#inlines () data;
          Printf.fprintf out "}"
        | Break_Line -> Printf.fprintf out "\\\\\n"
        | Entity e -> 
          if not e.latex_mathp then 
            Printf.fprintf out "%s" e.latex
          else
            Printf.fprintf out "$%s$" e.latex
        | Latex_Fragment (Inline.Math s) ->
          Printf.fprintf out "$%s$" (escape ["$"] s)
        | Latex_Fragment (Command (name, "")) ->
          Printf.fprintf out "\\%s" name
        | Latex_Fragment (Command (name, option)) ->
          Printf.fprintf out "\\%s{%s}" name (self#escape_inside option)
        | Verbatim s -> 
          Printf.fprintf out "\\texttt{%s}" (escape ["}"] s)
        | x -> super#inline () x
      method list_item () i = (match i.number with
        | Some c -> Printf.fprintf out "  \\item[%s] " c
        | _ -> Printf.fprintf out "\\item ");
        self#blocks () i.contents

      method block () = function
        | Paragraph l -> self#inlines () l; Printf.fprintf out "\n\n"
        | List (i, _) ->
          Printf.fprintf out "\\begin{itemize}\n";
          List.fold_left self#list_item () i;
          Printf.fprintf out "\\end{itemize}\n";

        | Math b ->
            Printf.fprintf out "$$%s$$\n" b
        | Custom (name, opts, l) ->
          Printf.fprintf out "\\begin{%s}{%s}\n" (self#escape_inside name)
            (self#escape_inside opts);
          self#blocks () l;
          Printf.fprintf out "\\end{%s}\n" (self#escape_inside name)
        | x -> super#block () x
      method heading () d = 
        let command = List.nth (Config.get sections) (d.level - 1) in
        Printf.fprintf out "\\%s{" command;
        self#inlines () d.name;
        Printf.fprintf out "}\n";
        self#blocks () d.content;
        List.iter (self#heading ()) d.children
      method document () d = 
        self#header ();
        super#document () d;
        self#footer ()
    end
    in o#document () doc

  let default_filename = change_ext "tex"
end

let _ = Modules.Exporters.add (module E : Exporters.Signature)        
