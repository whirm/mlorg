open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config
open Latex.E
module E = struct
  let name = "beamer"
  let config = Config.create ()
  let classname = 
    add config "classname" string "The LaTeX class name to use" "article"
  let header = add config "header" string
    "The LaTeX header."
~vars:
    [make_var "classname" "the class name chosen for this document";
     make_var "packages" "the list of packages to be loaded (formatted)";
     make_var "extraheader" "user's extra header (set by extraheader option)";
     make_var "title" "Document's title";
     make_var "author" "Document's author"]
"\\documentclass{$classname}
$packages
$extraheader
\\title{$title}
\\author{$author}
\\begin{document}
\\maketitle
"
  let footer = add config "footer" string "The LaTeX footer"
    "\\end{document}"
  let extraheader = add config "extraheader" string "Extra LaTeX header" ""
    
  let write_header config out doc =
    let vars = ["classname", escape_inside (Config.get config classname);
                "packages", "";
                "extraheader", Config.get config extraheader;
                "title", escape_inside doc.title;
                "author", escape_inside doc.author;
               ]
    in
    IO.nwrite out (substitute (assoc vars) (Config.get config header))
  class beamerExporter config out = object(self)
    inherit (Latex.E.latexExporter config out) as super
    method heading toc heading = 
      if heading.children = [] then
        (Printf.fprintf out "\\begin{frame}{";
         self#inlines toc heading.name;
         Printf.fprintf out "}\n";
         self#blocks toc heading.content;
         Printf.fprintf out "\\end{frame}\n")
      else
        super#heading toc heading
    method document toc document =
      write_header config out document;
      super#_document toc document;
      Printf.fprintf out "%s" (Config.get config footer)


  end
  let export config doc out = 
    (new beamerExporter config out)#document (Toc.gather config doc) doc

  module D = struct let export = export and default_filename = change_ext "tex" end
  type interface = exporter
  let data = (module D : Exporter)
end

let _ = Exporters.add (module E : Plugin with type interface = exporter)        
