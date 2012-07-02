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

(** {2 Tables} *)
and table = {
  groups : (int * int) list option;
  (** List of columns to group. A list of couple (start, stop) *)
  align_line : int array option;
  (** The size of each columns wanted by the user*)
  rows: Inline.t list array array;
  (** THe rows *)
  format : string option
  (** The table's format *)
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
  | Custom of string * string * t list
  | Drawer of t list
  | Property_Drawer of (string * string) list
  | Footnote_Definition of string * Inline.t list
  | Table of table
let map f v l = List.map (f v) l
class ['a] mapper = object(self)
  inherit ['a] Inline.mapper
  method blocks = map self#block 
  method block (v:'a) = function
    | List (l, b) -> List (map self#list_item v l, b)
    | Heading h -> Heading { h with title = self#inlines v h.title }
    | Paragraph i -> Paragraph (self#inlines v i)
    | Footnote_Definition (s, i) -> Footnote_Definition (s, self#inlines v i)
    | Custom (a, b, t) -> Custom (a, b, self#blocks v t)
    | Quote t -> Quote (self#blocks v t)
    | Table t -> Table {t with rows = 
        Array.map (Array.map (self#inlines v)) t.rows}
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
    | List (l, b) -> List.fold_left self#list_item v l
    | Paragraph i | Footnote_Definition (_, i) -> self#inlines v i 
    | Custom (_, _, t)
    | Quote t -> self#blocks v t
    | Table t -> 
        Array.fold_left (Array.fold_left self#inlines) v t.rows
    | (Drawer _ | Property_Drawer _ | Name _ | Src _ |
        Example _ | Math _ | Directive _) -> v
  method list_item v { contents } = self#blocks v contents
end
  
class virtual ['a] bottomUp = object(self)
  inherit ['a] Inline.bottomUp
  method blocks = self#combine -| List.map self#block
  method block = function
    | Heading h -> self#inlines h.title
    | List (l, b) -> self#combine (List.map self#list_item l)
    | Paragraph i | Footnote_Definition (_, i) -> self#inlines i 
    | Custom (_, _, t)
    | Quote t -> self#blocks t
    | Table t ->
        let f = self#combine -| Array.to_list in
        f (Array.map (f -| Array.map self#inlines) t.rows)
    | (Drawer _ | Property_Drawer _ | Name _ | Src _ |
        Example _ | Math _ | Directive _) -> self#bot
  method list_item { contents } = self#blocks contents
end
  
