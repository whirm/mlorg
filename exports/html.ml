open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config
open Xml
module E = struct
  let name = "html"
  let config = Config.create ()
  let encoding = Config.add config "encoding" string "The document's encoding" "utf-8"
  let full = Config.add config "wrap" boolean "Shall the output be a full html document ?" true
  let style = Config.add config "style" string "The stylesheet to use" "style.css"
  let use_math2png =  Config.add config "use-math2png" boolean "Convert latex formulas to PNG using Math2png extension" true
  let image_extensions = Config.add config "image-extensions" (list string) "The list of extensions to be considered as images"
    [".png"; ".jpg"; ".jpeg"; ".gif"; ".bmp"]
  type interface = exporter
  let concatmap f l = List.concat (List.map f l)
  let assoc l s = try List.assoc s l with _ -> ""
  class htmlExporter ({ get } as conf) = object(self)
    inherit [Xml.t list] Document.bottomUp as super
    method bot = []
    method combine = List.concat
    method wrap d contents = 
      let head = Xml.block "head"
        [Xml.block "title" [Xml.data d.title];
         Xml.block "meta" ~attr: ["http-equiv", "Content-Type"; "content", "text/html";
                                  "charset", get encoding] [];
         Xml.block "script" ~attr:["type","text/javascript"; 
                                   "src","http://orgmode.org/mathjax/MathJax.js"] [Xml.data " "];
         Xml.block "link" ~attr: ["rel", "stylesheet"; "href", get style;
                                    "type", "text/css"; "media", "screen"] []]
      in
      Xml.block "html"
        ~attr:["xmlns", "http://www.w3.org/1999/xhtml"]
        [head; Xml.block "body" contents]
    method inline = function
      | Plain s -> [Xml.data s]
      | Emphasis (kind, data) ->
          let l = [`Bold, "b"; `Italic, "i"; `Underline, "u"] in
          [Xml.block (List.assoc kind l) (self#inlines data)]
      | Entity e -> 
          [Xml.data e.html]
      | Latex_Fragment (Inline.Math s) -> [Xml.data ("\\("^s^"\\)")]
      | Link {url; label} ->
          let href = Inline.string_of_url url in
          if List.exists (String.ends_with href) (get image_extensions) then
            let opts, href = 
              print_endline href;
              try let (Complex {protocol; link}) = url in
                  Scanf.sscanf protocol "depth-%d" 
                    (fun n -> ["style", Printf.sprintf "vertical-align: -%dpx" n], link)
              with _ -> [], href
            in
            [Xml.block "img" ~attr: (opts @
                                       ["src", href;
                                        "title", Inline.asciis label]) []]
          else
            [Xml.block "a" ~attr: ["href", Inline.string_of_url url]
                (self#inlines label)]
      | Verbatim s -> 
          [Xml.block "code" [Xml.data s]]
      | x -> super#inline x
    method list_item x = 
      let contents = match x.contents with
        | [Paragraph i] -> self#inlines i
        | _ -> self#blocks x.contents
      in
      match x.number with
        | None -> [Xml.block "li" contents]
        | Some number ->
            [Xml.block ~attr: ["style", "list-style-type: none"] "li"
                (Xml.data number :: contents)]
    method block = function
      | Paragraph l -> [Xml.block "p" (self#inlines l)]
      | List (l, _) ->
          [Xml.block "ul" (concatmap (self#list_item) l)]
      | Example (_, l) ->
          [Xml.block "pre" [Xml.data (String.concat "\n" l)]]
      | Quote l ->
          [Xml.block "blockquote" (self#blocks l)]
      | x -> super#block x
    method heading d = 
      (Xml.block (Printf.sprintf "h%d" d.level)
         (self#inlines d.name)) ::
        (self#blocks d.content
         @ concatmap (self#heading) d.children)
    method document d =
      [Xml.block "div" ~attr:["id", "content"]
          (Xml.block "h1" ~attr:["class", "title"] 
          [Xml.data d.title] ::
          (self#blocks d.beginning @
             concatmap (self#heading) d.headings))]
  end


  let doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

  let with_custom_exporter o { get } doc out = 
    let doc = if get use_math2png then Math2png.transform { get } doc 
      else doc
    in
    if get full then
      (IO.nwrite out doctype;
       Xml.output_xhtml out [o#wrap doc (o#document doc)])
    else
      Xml.output_xhtml out (o#document doc)
  module E = struct
    let export c = with_custom_exporter (new htmlExporter c) c
    let default_filename = change_ext "html"
  end
  let data = (module E : Exporter)
end
let _ = Plugin.Exporters.add (module E : Plugin with type interface = exporter)
