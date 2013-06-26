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
  rows: Inline.t list array array array;
  (** The rows, split in groups *)
  format : string option
  (** The table's format *)
}
and code_block = {
  numbered: [ `Yes | `Keep] option;
  contents: (string * string option) list;
  format: 'a 'b 'c. (string -> 'a, 'b, 'c) format ;
  header_arguments: Hd_arguments.t;
}


  
and t = 
  | Paragraph of Inline.t list
  | Heading of heading
  | List of list_item list * bool
  | Directive of string * string
  | Math of string
  | Quote of t list
  | With_Keywords of (string * string) list * t
  | Example of int * string list
  | Src of int * string * string list
  | Custom of string * string * t list
  | Latex_Environment of string * string * string list
  | Drawer of string * t list
  | Property_Drawer of (string * string) list
  | Footnote_Definition of string * Inline.t list
  | Horizontal_Rule
  | Table of table
let map f v l = List.map (f v) l
class ['a] mapper = object(self)
  inherit ['a] Inline.mapper
  method blocks = map self#block 
  method block (v:'a) = function
    | List (l, b) -> List (map self#list_item v l, b)
    | Table t -> Table
        { t with rows = Array.map (Array.map (Array.map (self#inlines v))) t.rows }
    | Heading h -> Heading { h with title = self#inlines v h.title }
    | Paragraph i -> Paragraph (self#inlines v i)
    | Footnote_Definition (s, i) -> Footnote_Definition (s, self#inlines v i)
    | Custom (a, b, t) -> Custom (a, b, self#blocks v t)
    | Quote t -> Quote (self#blocks v t)
    | Drawer (name, t) -> Drawer (name, self#blocks v t)
    | With_Keywords (vals, l) -> With_Keywords (vals, self#block v l)
    | (Property_Drawer _ | Src _ | Latex_Environment _ | Horizontal_Rule
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
        Array.fold_left (Array.fold_left (Array.fold_left self#inlines))
          v t.rows
    | List (l, b) -> List.fold_left self#list_item v l
    | Paragraph i | Footnote_Definition (_, i) -> self#inlines v i 
    | With_Keywords (_, t) -> self#block v t
    | Custom (_, _, t)
    | Drawer (_, t) | Quote t -> self#blocks v t
    | (Property_Drawer _ | Src _ | Latex_Environment _  | Horizontal_Rule
          | Example _ | Math _ | Directive _) -> v
  method list_item v { contents } = self#blocks v contents
end
  
class virtual ['a] bottomUp = object(self)
  inherit ['a] Inline.bottomUp
  method blocks = self#combine % List.map self#block
  method block = function
    | Heading h -> self#inlines h.title
    | Table t ->
        let combine_arr f = self#combine % Array.to_list % Array.map f in
        combine_arr (combine_arr (combine_arr self#inlines)) t.rows 
    | List (l, b) -> self#combine (List.map self#list_item l)
    | Paragraph i | Footnote_Definition (_, i) -> self#inlines i 
    | With_Keywords (_, t) -> self#block t
    | Custom (_, _, t)
    | (Drawer (_, t) | Quote t) -> self#blocks t
    | (Property_Drawer _ | Src _ | Latex_Environment _  | Horizontal_Rule
          | Example _ | Math _ | Directive _) -> self#bot
  method list_item { contents } = self#blocks contents
end

class virtual ['a, 'b] bottomUpWithArg = object(self)
  inherit ['a, 'b] Inline.bottomUpWithArg
  method blocks arg = self#combine % List.map (self#block arg)
  method block arg = function
    | Heading h -> self#inlines arg h.title
    | Table t ->
        let combine_arr f = self#combine % Array.to_list % Array.map f in
        combine_arr (combine_arr (combine_arr (self#inlines arg))) t.rows 
    | List (l, b) -> self#combine (List.map (self#list_item arg) l)
    | Paragraph i | Footnote_Definition (_, i) -> self#inlines arg i 
    | With_Keywords (_, t) -> self#block arg t
    | Custom (_, _, t)
    | (Drawer (_, t) | Quote t) -> self#blocks arg t
    | (Property_Drawer _ | Src _ | Latex_Environment _  | Horizontal_Rule
          | Example _ | Math _ | Directive _) -> self#bot
  method list_item arg { contents } = self#blocks arg contents
end
  
