open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config

module L = struct
  let name = "latex"
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
    
  let sections = add config "sections" (list string)
    "The name of the sections"
    ["section"; "subsection"; "subsubsection"; "paragraph"; "subparagraph"]
  let pdfmeta = add config "pdf-meta" boolean
    "Insert PDF metadata for the author and the title (if set)" true
  let assoc l s = try List.assoc s l with _ -> ""

  let escape_inside s = s
  let tex_escape = escape ["}"; "{"; "$"; "\\"; "#"; "_"; "%";"^";]
  let write_header config out doc =
    let vars = ["classname", escape_inside (Config.get config classname);
                "packages", "\\usepackage{hyperref}";
                "extraheader", Config.get config extraheader;
                "title", escape_inside doc.title;
                "author", escape_inside doc.author;
               ]
    in
    let meta s s' = if s' = "" then "" else Printf.sprintf "%s={%s}" s (tex_escape s') in
    IO.nwrite out (substitute (assoc vars) (Config.get config header));
    Printf.fprintf out "\\hypersetup{%s,%s}\n" (meta "pdftitle" doc.title) (meta "pdfauthor" doc.author)

  class latexExporter config out = object(self)
      inherit [unit, Toc.t] Document.bottomUpWithArg as super
      val mutable footnote_stack = []
      val mutable footnote_defs = []
      method bot = ()
      method combine _ = ()
      method header doc = write_header config out doc
      method footer doc = 
        IO.nwrite out (Config.get config footer)
      method inline toc = function
        | Plain s -> IO.nwrite out (tex_escape s)
        | Emphasis (kind, data) ->
          let l = [`Bold, "textbf"; `Italic, "emph"; `Underline, "underline"] in
          Printf.fprintf out "\\%s{" (List.assoc kind l);
          self#inlines toc data;
          Printf.fprintf out "}"
        | Break_Line -> Printf.fprintf out "\\\\\n"
        | Footnote_Reference {Inline.name; Inline.definition} -> 
          (match definition with
            | Some u -> 
              Printf.fprintf out "\\footnote{"; 
              self#inlines toc u;
              Printf.fprintf out "}";
              footnote_stack <- footnote_stack @ [name]
            | None ->
                try
                  let body = List.assoc name footnote_defs in
                  try 
                    let (k, _) = List.findi (fun _ -> (=) name) footnote_stack in
                    Printf.fprintf out "\\footnotemark[%d]" (1+k);
                  with Not_found ->
                    footnote_stack <- footnote_stack @ [name];
                    Printf.fprintf out "\\footnote{";
                    self#inlines toc body;
                    Printf.fprintf out "}";
                with Not_found -> Log.warning "Reference to undefined footnote: %s" name)
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
          Printf.fprintf out "\\%s{%s}" name (tex_escape option)
        | Verbatim s -> 
          Printf.fprintf out "\\texttt{%s}" (tex_escape s)
        | Target s ->
          Printf.fprintf out "\\label{%s}" (Toc.link s)
        | Link {url; label} ->
            (match url, label with
              | Search title, _ ->
                Printf.fprintf out "\\hyperref"; 
                  if Toc.mem title toc then
                    Printf.fprintf out "[sec:%s]" (Toc.link title)
                  else
                    Printf.fprintf out "[%s]" (snd (String.replace ~str:title ~sub: "%20" ~by: " "));
                  Printf.fprintf out "{"; self#inlines toc label;
                  Printf.fprintf out "}"
              | _, label ->
                  Printf.fprintf out "\\href{%s}{"
                    (tex_escape (Inline.string_of_url url));
                  self#inlines toc label;
                  Printf.fprintf out "}")
        | x -> super#inline toc x
      method list_item toc i = (match i.number with
        | Some c -> Printf.fprintf out "  \\item[%s] " c
        | _ -> Printf.fprintf out "\\item ");
        self#blocks toc i.contents

      method block toc = function
        | Paragraph l -> self#inlines toc l; Printf.fprintf out "\n\n"
        | List (i, _) ->
          Printf.fprintf out "\\begin{itemize}\n";
          List.iter (self#list_item toc) i;
          Printf.fprintf out "\\end{itemize}\n";

        | Latex_Environment (name, opts, lines) ->
            Printf.fprintf out "\\begin{%s}%s\n" name opts;
            List.iter (Printf.fprintf out "%s\n") lines;
            Printf.fprintf out "\\end{%s}\n" name
        | Math b ->
            Printf.fprintf out "$$%s$$\n" b
        | Footnote_Definition _ -> ()
        | Custom ("tableofcontents", _, _) ->
            Printf.fprintf out "\\tableofcontents\n"
        | Custom (name, opts, l) ->
          Printf.fprintf out "\\begin{%s}%s\n" (escape_inside name)
            (escape_inside opts);
          self#blocks toc l;
          Printf.fprintf out "\\end{%s}\n" (escape_inside name)
        | With_Keywords (l, Custom ("figure", opts, lines)) ->
            Printf.fprintf out "\\begin{figure}%s\n\\centering" opts;
          self#blocks toc lines;
            (try Printf.fprintf out "\\caption{%s}\n" (List.assoc "CAPTION" l)
            with _ -> ());
          Printf.fprintf out "\\end{figure}\n"
            
        | x -> super#block toc x
      method heading toc d = 
        let command = List.nth (Config.get config sections) (d.level - 1) in
        let () = footnote_defs <- d.meta.footnotes in
        Printf.fprintf out "\\%s%s{" command 
          (if Document.has_tag "nonumber" d then "*" else "");
        self#inlines toc d.name;
        Printf.fprintf out "}\\label{sec:%s}\n" (Toc.link (Inline.asciis d.name));
        self#blocks toc d.content;
        List.iter (self#heading toc) d.children
      method _document toc d = 
        footnote_defs <- d.beg_meta.footnotes;
        super#document toc d;
      method document toc d = 
        self#header d;
        self#_document toc d;
        self#footer d
    end

  let export config doc out = 
    (new latexExporter config out)#document (Toc.gather config doc) doc

  module D = struct let export = export and default_filename = change_ext "tex" end
  type interface = exporter
  let data = (module D : Exporter)
end

module Config = struct
  include L
end
include L
let _ = Exporters.add (module L : Plugin with type interface = exporter)        
