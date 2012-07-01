(** Definition of blocks *)


(**
This file is dealing with parsing top-level entities in org-mode files such as
lists, paragraphs, tables, codes, headings. In this file is only given the type
definitions, and a few parsing functions for specific things.

This module doesn't cover all of the org-mode formatting.

For now it covers :
- headings
- paragraphs
- lists
*)

(** {1 Type definition} *)

(** {2 Headings} *)
type heading = {
  title: Inline.t list;
  tags: string list;
  marker: string option; (** TODO, DONE, and so on *)
  level: int;
  priority: char option
}
(** The data for a heading. *)
(** {2 Lists} *)
and list_item = {
  contents: t list;
  number: string option; (** The optional number of the item *)
  checkbox: bool option; (** Does it have a checkbox ([[ ]]) and is it checked ? *)
}
(** A list item *)

(** {2 Blocks} *)
and t = 
  | Paragraph of Inline.t list
  (** A paragraph containing only inline text *)
  | Heading of heading
  (** A heading *)
  | List of list_item list * bool
  (** A list [item, ordered?] *)
  | Directive of string * string
  (** A directive [name, value] *)
  | Math of string
  (** Math, enclosed by $$ ... $$ *)
  | Quote of t list
  (** Quoted text *)
  | Name of string
  (** A name tag that maps to next block *)
  | Example of int * string list
  (** [Examples] used to typeset random code snippets. The integer is the line number in the source file. *)
  | Src of int * string * string list
  (** [Src] is used to typeset code snippets. The integer is the line number in the source file. *)
  | Custom of string * string * t list
  (** Custom block of the form
  #+begin_name opts
      DATA
  #+end *)
  | Drawer of string list
  (** A drawer *)
  | Property_Drawer of (string * string) list
  (** A property drawer *)
  | Table of table
(** Table *)
and table = {
  groups : (int * int) list option;
  (** List of columns to group. A list of couple (start, stop) *)
  align_line : int array option;
  (** The size of each columns wanted by the user*)
  rows: Inline.t list array array;
  (** The rows *)
  format : string option
  (** The table's format *)
}

(** {1 Mapper and folders} *)
class ['a] mapper : object
  inherit ['a] Inline.mapper
  method block : 'a -> t -> t
  method blocks : 'a -> t list -> t list
  method list_item : 'a -> list_item -> list_item
end

class ['a] folder : object
  inherit ['a] Inline.folder
  method block : 'a -> t -> 'a
  method blocks : 'a -> t list -> 'a
  method list_item : 'a -> list_item -> 'a
end

class virtual ['a] bottomUp : object
  inherit ['a] Inline.bottomUp
  method block : t -> 'a
  method blocks : t list -> 'a
  method list_item : list_item -> 'a
end
