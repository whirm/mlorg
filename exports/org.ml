(* Org.ml: Outputs to org *)
open Timestamp
open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config
module E = struct
  let name = "org"
  let config = Config.create ()

  let wrap_brace ?(c1="{") ?(c2="}") = Option.map_default
    (fun s -> Printf.sprintf "%s%s%s" c1 (escape [c1;c2] s) c2) ""
  let wrap_bracket = wrap_brace ~c1:"[" ~c2:"]"
  let range x = Timestamp.range_to_string x
  let timestamp x = Timestamp.to_string x

  let ws = Printf.sprintf
  let rec inline = function
    | Plain s -> 
      ws "%s" (escape ["/"; "="; "*"; "_"; "{"; "["; "]"; "}"] s)
    | Emphasis (kind, data) ->
      let s = List.assoc kind [`Bold, "*"; `Italic, "/"; `Underline, "_"] in
      ws "%s%s%s" s (inlines data) s
    | Entity e -> ws "\\%s" e.Entity.name
    | Export_Snippet (backend, text) ->
      ws "@%s{%s}" backend (escape ["}"; "{"] text)
    | Footnote_Reference {Inline.name; definition} ->
      (match definition with
      | Some def -> ws "[fn:%s:%s]" name (inlines def);
          | None -> ws "[fn:%s]" name)
    | Inline_Call {program; arguments; inside_headers; end_headers} ->
        let arguments = List.map (fun (a, b) -> Printf.sprintf "%s=%s" a b) arguments
    |> String.concat ","
        in
        ws "call_%s%s(%s)%s" program 
          (wrap_bracket inside_headers)
          (escape ["("; ")"] arguments) (wrap_bracket end_headers)
	  
    | Inline_Source_Block {language; options; code} ->
      ws "src_%s%s%s" language (wrap_bracket options) 
        (wrap_brace (Some code))
    | Subscript t -> 
      ws "_{%s}" (inlines t)
    | Superscript t -> 
      ws "^{%s}" (inlines t)
    | Macro (name, args) -> 
      ws "{{{%s(%s)}}}"
        (escape ["{"; "}"] name)
        (String.concat ", " (List.map (escape ["{"; "}"]) args))
    | Latex_Fragment (Inline.Math s) -> 
      ws "$%s$" (escape ["$"] s)
	
    | Latex_Fragment (Inline.Command (opt, s)) -> 
      ws "\\%s%s" s (wrap_brace (if opt = "" then None else Some opt))
    | Link {url=Search search; label} when Inline.asciis label = search -> 
      ws "[[%s]]" (inlines label)
    | Link {url; label} ->
      ws "[[%s][%s]]" (Inline.string_of_url url) (inlines label)
     | Break_Line -> ws "\\\\\n"
     | Target s -> ws "<<%s>>" s
     | Radio_Target s -> ws "<<<%s>>>" s
     | Cookie (Percent k) -> ws "[%d%%]" k
     | Cookie (Absolute (k, k')) -> ws "[%d/%d]" k k'
     | Timestamp (Scheduled t) -> ws "SCHEDULED: %s" (timestamp t)
     | Timestamp (Deadline t) -> ws "DEADLINE: %s" (timestamp t)
     | Timestamp (Date t) -> ws "%s" (timestamp t)
     | Timestamp (Range t) -> ws "%s" (range t)
     | Timestamp (Closed t) -> ws "CLOSED: %s" (timestamp t)
     | Timestamp (Clock (Stopped t)) -> ws "CLOCK: %s\n" (range t)
     | Timestamp (Clock (Started t)) -> ws "CLOCK: %s\n" (timestamp t)
     | Verbatim s -> ws "=%s=" (escape ["="] s)
  and inlines x = List.map inline x |> String.concat ""
  let inline_to_string = inlines
  class orgExporter out = 
    let indent_level = ref 0 in
    let ws fmt = Printf.kprintf (fun s -> match lines s with
      | [] -> ()
      | [t] -> IO.nwrite out t
      | t :: q -> IO.nwrite out t; 
        List.iter (fun s ->
          IO.write out '\n';
          for k = 1 to !indent_level do IO.write out ' ' done;
          IO.nwrite out s)
          q) fmt
          
    in object(self)
    inherit [unit] Document.bottomUp as super
    method bot = ()
    method combine _ = ()
    method inline = inline %> ws "%s"
    method range = range %> ws "%s"
    method timestamp = timestamp %> ws "%s"
    method list_item x = (match x.number with
      | None -> ws "- "; 
      | Some number -> ws "1. [@%s] " number);
      (match x.checkbox with
        | Some b -> ws "[%c] " (if b then 'X' else ' ')
        | None -> ());
      indent_level := !indent_level + 2;
      self#blocks x.contents;
      indent_level := !indent_level - 2;
    method blocks = function
    | [] -> ()
    | [t] -> self#block t
    | t :: q -> self#block t; Printf.fprintf out "\n"; self#blocks q
    method block = function
      | Paragraph l -> self#inlines l; Printf.fprintf out "\n"
      | Heading _ -> () (* heading is handled in the heading method *)
      | List (l, _) -> List.iter self#list_item l
      | Directive (a, b) -> ws "#+%s: %s\n" a b
      | Math s -> ws "$$ %s $$\n" s
      | Quote l ->
        ws "#+begin_quote\n";
        self#blocks l;
        ws "#+end_quote\n";
      | With_Keywords (l, b) -> 
        List.iter (fun (name, v) -> ws "#+%s: %s\n" name v) l;
        self#block b
      | Example (_, lines) ->
        List.iter (ws ": %s\n") lines
      | Src (number, opts, lines) ->
        ws "#+begin_src %s\n" opts;
        List.iter (ws "%s\n") lines;
        ws "#+end_src\n"
      | Custom (name, opt, contents) ->
        ws "#+begin_%s %s\n" name opt;
        self#blocks contents;
        ws "#+end_%s\n" name
      | Drawer (name, c) ->
        ws ":%s:\n" name;
        self#blocks c;
        ws ":END:\n"
      | Property_Drawer l -> 
        ws ":PROPERTIES:\n";
        List.iter (fun (name, v) -> ws ":%s: %s\n" name v) l;
        ws ":END:\n";
      | Table t ->
        let pr_row = Array.iter (fun x -> ws "| "; self#inlines x) in
        Array.iter (fun x -> pr_row x; ws " |\n") t.rows;
        Option.map_default (ws "#+tblfm: %s") () t.format
      | Horizontal_Rule -> ws "-----\n"
      | Latex_Environment (name, opts, contents) ->
        ws "\\begin{%s}%s\n%s\n\n\n\\end{%s}\n" name opts
          (String.concat "\n" contents) name
      | Footnote_Definition (name, contents) ->
        ws "[%s] " name;
        self#inlines contents
    method heading d = 
      let iter f = Option.map_default f () in 
      for i = 1 to d.level do ws "*" done;
      ws " ";
      iter (ws "[#%c] ") d.Document.priority;
      iter (ws "%s ") d.marker;
      self#inlines d.name;
      if d.tags <> [] then
        ws "     :%s:" (String.concat ":" d.tags);
      ws "\n";
      self#blocks d.Document.content;
      List.iter self#heading d.children
    end
  
module Exp = struct
    let export _ doc out = 
      (new orgExporter out)#document doc
    let default_filename = change_ext "org"
  end
  type interface = exporter
  let data = (module Exp : Exporter)
end
let _ = Exporters.add (module E : Plugin with type interface = exporter)        
include E

let export = Exp.export

module Config = struct

end
