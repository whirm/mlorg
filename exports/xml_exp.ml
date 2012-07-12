open Timestamp
open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Modules
open Config
open Xml
module E = struct
  module Meta = struct
    let name = "xml"
    let config = Config.create ()
    let config = Config.validate config
  end
  open Meta

  let concatmap f l = List.concat (List.map f l)
  let assoc l s = try List.assoc s l with _ -> ""
  let opt_attr name = function
    | Some v -> [name, v]
    | None -> []
  class xmlExporter = object(self)
    inherit [Xml.t list] Document.bottomUp as super
    method bot = []
    method combine = List.concat
    method inline = function
      | Plain s -> [Xml.data s]
      | Emphasis (kind, data) ->
          let l = [`Bold, "bold"; `Italic, "italic"; `Underline, "underline"] in
          [Xml.block (List.assoc kind l) (self#inlines data)]
      | Entity e -> 
          [Xml.data e.unicode]
      | Export_Snippet (backend, text) ->
          [Xml.block "export-snippet" ~attr:["backend", backend]
              [Xml.data text]]
      | Footnote_Reference {Inline.name; definition} ->
          [Xml.block "footnote-reference" ~attr: (opt_attr "name" name)
              (Option.map_default self#inlines [] definition)]
      | Inline_Call {program; arguments; inside_headers; end_headers} ->
          [Xml.block "inline-call" ~attr: (["program", program]
                                           @ opt_attr "inside-headers" inside_headers
                                           @ opt_attr "end-headers" end_headers)
              (List.map (fun (k, v) -> 
                Xml.block "parameter" ~attr:["key", k; "value", v] [])
                 arguments)]

      | Inline_Source_Block {language; options; code} ->
          [Xml.block "inline-source" ~attr:(["language", language]
                                            @ opt_attr "options" options)
              [Xml.data code]]

            
      | Subscript t -> [Xml.block "subscript" (self#inlines t)]                               
      | Superscript t -> [Xml.block "superscript" (self#inlines t)]                               
      | Macro (name, args) -> 
          [Xml.block "macro" ~attr:["name", name]
              (List.map (fun v -> 
                Xml.block "parameter" ~attr:["value", v] [])
                 args)]
      | Latex_Fragment (Inline.Math s) -> 
          [Xml.block "math-inline" [Xml.data s]] 
      | Latex_Fragment (Inline.Command (opt, s)) -> 
          [Xml.block "latex-command" ~attr: ["option", opt] [Xml.data s]]
     | Link {url; label} ->
          [Xml.block "link" ~attr: ["href", Inline.string_of_url url]
              (self#inlines label)]
     | Break_Line -> [Xml.block "break-line" []]
     | Cookie (Percent k) -> [Xml.block "cookie" ~attr:["percent", string_of_int k] []]
     | Cookie (Absolute (k, k')) -> 
         [Xml.block "cookie" ~attr:["done", string_of_int k; "total", string_of_int k'] []]
     | Timestamp (Scheduled t) -> [Xml.block "scheduled" [self#timestamp t]]
     | Timestamp (Deadline t) -> [Xml.block "deadline" [self#timestamp t]]
     | Timestamp (Date t) -> [Xml.block "date" [self#timestamp t]]
     | Timestamp (Range t) -> [Xml.block "range" [self#range t]]
     | Radio_Target s -> [Xml.block "radio-target" [Xml.data s]]
     | Verbatim s -> 
         [Xml.block "verbatim-inline" [Xml.data s]]
    method range {start; stop} =
      Xml.block "time-range"
        [Xml.block "start" [self#timestamp start];
         Xml.block "stop" [self#timestamp stop]]
    method date {year; month; day} = 
      Xml.block "dateitem" ~attr:["year", string_of_int year;
                                  "month", string_of_int month;
                                  "day", string_of_int day] []
    method time {hour; min} = 
      Xml.block "time" ~attr:["hour", string_of_int hour;
                              "min", string_of_int min] []
    method timestamp {active; date; time; repetition} = 
      Xml.block "timestamp" ~attr:["active", if active then "true" else "false"]
        [self#date date; Option.map_default self#time Xml.empty time;
         Option.map_default (Xml.block "repetition" -| (fun x -> [x]) -| self#date)
           Xml.empty repetition]
    method list_item x = 
      [Xml.block "item"
        ~attr: (opt_attr "number" x.number)
        (self#blocks x.contents)]

    method block = function
      | Paragraph l -> [Xml.block "paragraph" (self#inlines l)]
      | Heading _ -> []
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
      let children = Xml.block "meta"
        [Xml.block "name" (self#inlines d.name);
          mk_list "scheduled" self#timestamp d.meta.scheduled;
          mk_list "deadlines" self#timestamp d.meta.deadlines;
          mk_list "timestamps" self#timestamp d.meta.timestamps;
          mk_list "range" self#range d.meta.ranges;
          mk_list "footnotes" self#footnote d.meta.footnotes;
          mk_list "properties" self#property d.meta.properties] ::
          (self#blocks d.content
           @ concatmap self#heading d.children)
      in [Xml.block "heading" children]
    method document d =
      [Xml.block "document" 
          ~attr: ["title", d.title; "author", d.author;
                  "filename", d.filename]
          (self#blocks d.beginning @
             concatmap self#heading d.headings)]
  end
  let export _ doc out = 
    Xml.output out
      ["underline"; "bold"; "italic"; "math"; "link"; "verbatim-inline";
       "footnote-reference"; "inline-source"; "inline-call"]
      ["paragraph"; "name"; "named-block"]
      []
      ["verbatim-inline"; "example"; "source"]
      ((new xmlExporter)#document doc)
  let default_filename = change_ext "xml"
end
let _ = Modules.Exporters.add (module E : Exporters.Signature)        
