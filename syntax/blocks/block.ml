type heading = {
  title: Inline.t list;
  tags: string list;
  marker: string option; (** TODO, DONE, and so on *)
  level: int;
  priority: char option
}
and list_item = {
  contents: t list;
  number: string option; (** The optional number of the item *)
  checkbox: bool option; (** Does it have a checkbox ([[ ]]) and is it checked ? *)
}
and t = 
  | Paragraph of Inline.t list
  | Heading of heading
  | List of list_item list * bool
  | Directive of string * string

let map f v l = List.map (f v) l
class ['a] mapper = object(self)
  inherit ['a] Inline.mapper
  method blocks = map self#block 
  method block (v:'a) = function
    | List (l, b) -> List (map self#list_item v l, b)
    | Heading h -> Heading { h with title = map self#inline v h.title }
    | Paragraph i -> Paragraph (map self#inline v i)
    | (Directive _ as x) -> x
  method list_item v ({ contents } as x) =
    { x with contents = self#blocks v contents }
end
  
class ['a] folder = object(self)
  inherit ['a] Inline.folder
  method blocks = List.fold_left self#block
  method block (v:'a) = function
    | Heading h -> self#inline_list v h.title
    | List (l, b) -> List.fold_left self#list_item v l
    | Paragraph i -> List.fold_left self#inline v i 
    | (Directive _) -> v
  method list_item v { contents } = self#blocks v contents
end
  