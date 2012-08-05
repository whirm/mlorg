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
    
  class orgExporter out = 
    let indent_level = ref 0 in
    let wrap_bracket = Option.map_default
      (fun s -> Printf.sprintf "{%s}" (escape ["{"; "}"] s)) "" in
    let ws fmt = Printf.kprintf (fun s -> match lines s with
      | [] -> ()
      | [t] -> IO.nwrite out t
      | t :: q -> IO.nwrite out t; IO.write out '\n';
        List.iter (fun s ->
          for k = 1 to !indent_level do IO.write out ' ' done;
          IO.nwrite out s)
          q) fmt
          
    in object(self)
    inherit [unit] Document.bottomUp as super
    method bot = ()
    method combine _ = ()
    method inline = function
      | Plain s -> 
        ws "%s" (escape ["/"; "="; "*"; "_"; "{"; "["; "]"; "}"] s)
      | Emphasis (kind, data) ->
          let s = List.assoc kind [`Bold, "*"; `Italic, "/"; `Underline, "_"] in
          IO.nwrite out s;
          self#inlines data;
          IO.nwrite out s;
      | Entity e -> IO.nwrite out ("\\"^e.Entity.name)
      | Export_Snippet (backend, text) ->
        Printf.fprintf out "@%s{" backend;
        ws "%s" (escape ["}"; "{"] text);
        IO.nwrite out "}"
      | Footnote_Reference {Inline.name; definition} ->
        (match definition with
          | Some def -> ws "[fn:%s:" name;
            self#inlines def;
            Printf.fprintf out "]"
          | None -> ws "[fn:%s]" name)
      | Inline_Call {program; arguments; inside_headers; end_headers} ->
        let arguments = List.map (fun (a, b) -> Printf.sprintf "%s=%s" a b) arguments
                        |> String.concat ", "
        in
        ws "call_%s%s(%s)%s" program 
          (wrap_bracket inside_headers)
          (escape ["("; ")"] arguments) (wrap_bracket end_headers)

      | Inline_Source_Block {language; options; code} ->
        ws "src_%s%s[%s]" language (wrap_bracket options) 
          (escape ["["; "]"] code)
      | Subscript t -> 
        IO.nwrite out "_{"; self#inlines t; IO.nwrite out "}"
      | Superscript t -> 
        IO.nwrite out "^{"; self#inlines t; IO.nwrite out "}"
      | Macro (name, args) -> 
        ws "{{{%s(%s)}}}"
          (escape ["{"; "}"] name)
          (String.concat ", " (List.map (escape ["{"; "}"]) args))
      | Latex_Fragment (Inline.Math s) -> 
        ws "$%s$" (escape ["$"] s)

      | Latex_Fragment (Inline.Command (opt, s)) -> 
        ws "\\%s%s" s (wrap_bracket (if opt = "" then None else Some opt))
      | Link {url; label} ->
        ws "[[%s][" (Inline.string_of_url url); 
        self#inlines label; ws "]]"
     | Break_Line -> ws "\\\\\n"
     | Target s -> ws "<<%s>>" s
     | Radio_Target s -> ws "<<<%s>>>" s
     | Cookie (Percent k) -> ws "[%d%%]" k
     | Cookie (Absolute (k, k')) -> ws "[%d/%d]" k k'
     | Timestamp (Scheduled t) -> ws "SCHEDULED: %s" (self#timestamp t)
     | Timestamp (Deadline t) -> ws "DEADLINE: %s" (self#timestamp t)
     | Timestamp (Date t) -> ws "%s" (self#timestamp t)
     | Timestamp (Range t) -> ws "%s" (self#range t)
     | Timestamp (Closed t) -> ws "CLOSED: %s" (self#timestamp t)
     | Timestamp (Clock (Stopped t)) -> ws "CLOCK: %s\n" (self#range t)
     | Timestamp (Clock (Started t)) -> ws "CLOCK: %s\n" (self#timestamp t)
     | Verbatim s -> ws "=%s=" (escape ["="] s)
    method range x = Timestamp.range_to_string x
    method timestamp x = Timestamp.to_string x
    method list_item x = (match x.number with
      | None -> ws "\n- "; 
      | Some number -> ws "1. [@%s] " number);
      (match x.checkbox with
        | Some b -> ws "[%c] " (if b then 'X' else ' ')
        | None -> ());
      indent_level := !indent_level + 2;
      self#blocks x.contents;
      indent_level := !indent_level - 2;
    method block = function
      | Paragraph l -> self#inlines l; IO.write out '\n'
      | x -> super#block x
(*      | Heading _ -> []
      | List (l, _) ->
          [Xml.block "list" (concatmap (self#list_item) l)]
      | Directive _ -> []
      | Math s -> [Xml.block "math-block" [Xml.data s]]
      | Quote l ->
          [Xml.block "quote" (self#blocks l)]
      | With_Keywords (l, b) -> [Xml.block "with-keywords" ~attr: l (self#block b)]
      | Example (line, l) ->
          [Xml.block "example" ~attr: ["linenumber", string_of_int line]
              [Xml.data (String.concat "\n" l)]]
      | Src (number, opts, lines) ->
          [Xml.block "source" ~attr:["options", opts; "linenumber", string_of_int number]
              [Xml.data (String.concat "\n" lines)]]
      | Custom (name, opt, contents) ->
          [Xml.block "custom" ~attr:["name", name; "options", opt]
              (self#blocks contents)]
      | Latex_Environment (name, opts, contents) ->
          [Xml.block "latex-environment" ~attr: ["name", name; "opts", opts]
              [Xml.data (String.concat "\n" contents)]]
      | Drawer (name, c) ->
          [Xml.block "drawer" ~attr:["name", name]
              (self#blocks c)]
      | Property_Drawer _ -> []
      | Table t ->
          let index = Option.map_default
            (Xml.block "sizes" -| Array.to_list -| 
                Array.mapi (fun i v -> Xml.block "index" 
                  ~attr: ["index", string_of_int i; "size", string_of_int v] []))
            Xml.empty t.align_line
          in
          let groups = Option.map_default
            (Xml.block "groups" -| 
                List.mapi (fun i (start, stop) -> Xml.block "group" 
                  ~attr: ["index", string_of_int i; 
                          "start", string_of_int start;
                          "stop", string_of_int stop] []))
            Xml.empty t.groups
          in
          let contents = Array.to_list
            (Array.map (Xml.block "row"
                           -| Array.to_list 
                           -| Array.map (Xml.block "cell" -| self#inlines))
            t.rows)
          in    
          [Xml.block "table" ~attr: (opt_attr "format" t.format)
              (index :: groups :: contents)]
     | Horizontal_Rule -> [Xml.block "horizontal-rule" []]
     | Footnote_Definition (name, contents) ->
       [Xml.block "footnote-definition" ~attr:["name", name]
           (self#inlines contents)]

    method footnote (name, contents) = 
      Xml.block "footnote" ~attr: ["name", name] (self#inlines contents)
    method property (key, value) = 
      Xml.block "properties" ~attr:["name", key; "value", value] []
    method heading d = 
      let mk_list name f l = if l = [] then Xml.empty
        else Xml.block name (List.map f l)
      in
      let attr = ["level", string_of_int d.level] @ opt_attr "marker" d.marker in
      let children = Xml.block "meta"
        [Xml.block "name" (self#inlines d.name);
          mk_list "scheduled" self#timestamp d.meta.scheduled;
          mk_list "deadlines" self#timestamp d.meta.deadlines;
          mk_list "timestamps" self#timestamp d.meta.timestamps;
          mk_list "range" self#range d.meta.ranges;
          mk_list "footnotes" self#footnote d.meta.footnotes;
          mk_list "clocks" self#range d.meta.clocks;
          (match d.meta.current_clock with
            | Some t -> Xml.block "current-clock" [self#timestamp t]
            | None -> Xml.empty);
          mk_list "properties" self#property d.meta.properties] ::
          (self#blocks d.content
           @ concatmap self#heading d.children)
      in [Xml.block "heading" ~attr children]
    method document d =
      [Xml.block "document" 
          ~attr: ["title", d.title; "author", d.author;
                  "filename", d.filename]
          (self#blocks d.beginning @
             concatmap self#heading d.headings)]*)
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
