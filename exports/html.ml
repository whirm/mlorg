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
  end
  open Meta
  let assoc l s = try List.assoc s l with _ -> ""
  let export doc out = 
    let o = object(self)
      inherit [Xml.t list] Document.folder as super
      method inline _ = function
        | Plain s -> [Xml.data (Xml.escape_entities s)]
        | Emphasis (kind, data) ->
          let l = [`Bold, "b"; `Italic, "i"; `Underline, "u"] in
          [Xml.block (List.assoc kind l) (self#inlines data)]
        | Entity e -> 
          [Xml.data e.html]
        | Verbatim s -> 
          [Xml.block "code" [Xml.data (Xml.escape_entities s)]]
        | x -> super#inline [] x
      method inlines l = 
        List.concat (List.map (self#inline []) l)
      method block _ = function
        | Paragraph l -> [Xml.block "p" (self#inlines l)]
        | x -> super#block [] x
      method heading _ d = 
        (Xml.block (Printf.sprintf "h%d" d.level)
            (self#inlines d.name)) ::
          (self#blocks [] d.content
           @ List.concat (List.map (self#heading []) d.children))
    end
    in 
    List.iter (Xml.output out) (o#document [] doc)
end

let _ = Modules.Exporters.add (module E : Exporters.Signature)        
