open Batteries
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
and table = {
  rows : Inline.t list array array; (** The rows of the table *)
  align_line : int array option; (** An optional size for each column *)
  groups : (int * int) list option; (** An optional grouping for columns. A list of [(index_open, index_close)] *)
  format : string option; (** The format of the table *)
}
and t = 
  | Paragraph of Inline.t list
  | Heading of heading
  | List of list_item list * bool
  | Directive of string * string
  | Math of string
  | Quote of t list
  | Name of string
  | Example of int * string list
  | Src of int * string * string list
  | Table of table
  | Custom of string * string * t list
  | Drawer of string list
  | Property_Drawer of (string * string) list

let map f v l = List.map (f v) l
class ['a] mapper = object(self)
  inherit ['a] Inline.mapper
  method blocks = map self#block 
  method block (v:'a) = function
    | List (l, b) -> List (map self#list_item v l, b)
    | Table t -> Table
        { t with rows = Array.map (Array.map (self#inlines v)) t.rows }
    | Heading h -> Heading { h with title = self#inlines v h.title }
    | Paragraph i -> Paragraph (self#inlines v i)
    | Custom (a, b, t) -> Custom (a, b, self#blocks v t)
    | Quote t -> Quote (self#blocks v t)
    | (Drawer _ | Property_Drawer _ | Name _ | Src _
          | Example _ | Math _ | Directive _ as x) -> x
  method list_item v ({ contents } as x) =
    { x with contents = self#blocks v contents }
end
  
class ['a] folder = object(self)
  inherit ['a] Inline.folder
  method blocks = List.fold_left self#block
  method block (v:'a) = function
    | Heading h -> self#inlines v h.title
    | Table t ->
        Array.fold_left (Array.fold_left self#inlines)
          v t.rows
    | List (l, b) -> List.fold_left self#list_item v l
    | Paragraph i -> self#inlines v i 
    | Custom (_, _, t)
    | Quote t -> self#blocks v t
    | (Drawer _ | Property_Drawer _ | Name _ | Src _ |
        Example _ | Math _ | Directive _) -> v
  method list_item v { contents } = self#blocks v contents
end
  
class virtual ['a] bottomUp = object(self)
  inherit ['a] Inline.bottomUp
  method blocks = self#combine -| List.map self#block
  method block = function
    | Heading h -> self#inlines h.title
    | Table t ->
        let combine_arr f = self#combine -| Array.to_list -| Array.map f in
        combine_arr (combine_arr self#inlines) t.rows 
    | List (l, b) -> self#combine (List.map self#list_item l)
    | Paragraph i -> self#inlines i 
    | Custom (_, _, t)
    | Quote t -> self#blocks t
    | (Drawer _ | Property_Drawer _ | Name _ | Src _ |
        Example _ | Math _ | Directive _) -> self#bot
  method list_item { contents } = self#blocks contents
end
  
