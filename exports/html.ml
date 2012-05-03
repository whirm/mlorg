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
    let name = "html"
    let config = Config.create ()
    let encoding = Config.add config "encoding" string "The document's encoding" "utf-8"
    let full = Config.add config "wrap" boolean "Shall the output be a full html document ?" true
    let style = Config.add config "style" string "The stylesheet to use" "style.css"
  end
  open Meta
  let concatmap f l = List.concat (List.map f l)
  let assoc l s = try List.assoc s l with _ -> ""
  let export doc out = 
    let o = object(self)
      inherit [Xml.t list] Document.folder as super
      method wrap d contents = 
        let head = Xml.block "head"
          [Xml.block "title" [Xml.data (Xml.escape_entities d.title)];
           Xml.block "meta" ~attr: ["http-equiv", "Content-Type"; "content", "text/html";
                                   "charset", Config.get encoding] [];
           Xml.block "script" ~attr:["type","text/javascript"; 
                                     "src","http://orgmode.org/mathjax/MathJax.js"] [Xml.data " "];
           Xml.block "link" ~attr: ["rel", "stylesheet"; "href", Config.get style;
                                    "type", "text/css"; "media", "screen"] []]
        in
        Xml.block "html"
          ~attr:["xmlns", "http://www.w3.org/1999/xhtml"]
          [head; Xml.block "body" contents]
      method inline _ = function
        | Plain s -> [Xml.data (Xml.escape_entities s)]
        | Emphasis (kind, data) ->
          let l = [`Bold, "b"; `Italic, "i"; `Underline, "u"] in
          [Xml.block (List.assoc kind l) (self#inlines data)]
        | Entity e -> 
          [Xml.data e.html]
        | Latex_Fragment (Inline.Math s) -> [Xml.data (Xml.escape_entities ("\\("^s^"\\)"))]
        | Verbatim s -> 
          [Xml.block "code" [Xml.data (Xml.escape_entities s)]]
        | x -> super#inline [] x
      method inlines l = 
        concatmap (self#inline []) l
      method list_item _ x = 
        let contents = match x.contents with
          | [Paragraph i] -> self#inlines i
          | _ -> self#blocks [] x.contents
        in
        match x.number with
        | None -> [Xml.block "li" contents]
        | Some number ->
          [Xml.block ~attr: ["style", "list-style-type: none"] "li"
            (Xml.data number :: contents)]
      method block _ = function
        | Paragraph l -> [Xml.block "p" (self#inlines l)]
        | List (l, false) ->
          [Xml.block "ul" (concatmap (self#list_item []) l)]
        | x -> super#block [] x
      method blocks _ l = 
        concatmap (self#block []) l
      method heading _ d = 
        (Xml.block (Printf.sprintf "h%d" d.level)
            (self#inlines d.name)) ::
          (self#blocks [] d.content
           @ concatmap (self#heading []) d.children)
      method document _ d =
        [Xml.block "div" ~attr:["id", "content"]
            (self#blocks [] d.beginning @
               concatmap (self#heading []) d.headings)]
    end
    in 
    if Config.get full then
      (IO.printf out "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">";
       Xml.output out (o#wrap doc (o#document [] doc)))
    else
      List.iter (Xml.output out) (o#document [] doc)
  let default_filename = change_ext "html"
end
let _ = Modules.Exporters.add (module E : Exporters.Signature)        
