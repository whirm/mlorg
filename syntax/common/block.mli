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
  title: Inline.t list; (** The title as inline formatted content *)
  tags: string list; (** The tags set by the user *)
  marker: string option; (** TODO, DONE, and so on *)
  level: int; (** The level (numbero of stars) -- starts at 1 *)
  priority: char option (** The optional priority *)
}
(** The data for a heading. *)

(** {2 Lists} *)

and list_item = {
  contents: t list; (** The item's contents *)
  number: string option; (** The optional number of the item *)
  checkbox: bool option; (** Does it have a checkbox ([[ ]]) and is it checked ? *)
}
(** A list item *)

(** {2 Tables} *)

and table = {
  groups : (int * int) list option;
  (** List of columns to group. A list of couple (start, stop) *)
  align_line : int array option;
  (** The size of each columns wanted by the user.  The size refers to
      the size of the column in character the editor should preserve
      when reformatting the table *)
  rows: Inline.t list array array array;
  (** The rows, grouped *)
  format : string option
(** The table's format *)
}
(** Tables *)

(** {2 Code blocks} *)
and code_block = {
  numbering: [ `Yes | `Keep] option; (** Is it to be numbered ? *)
  lines: (string * string option) list; (** The contents as a list of (lines, ref to that line) *)
  ref_format: 'a 'b. (string -> 'a, 'b, 'a, 'a) format4 ; (** The format used to parse ref *)
  header_arguments: Hd_arguments.t; (** The header arguments *)
  language: string; (** The language the code is written in *)
  linenumber: int; (** The line number it starts at *)
}
(** Code blocks *)

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
| With_Keywords of (string * string) list * t
  (** Keywords for a block *)
| Example of int * string list
  (** [Examples] used to typeset random code snippets. The integer is the line number in the source file. *)
| Src of code_block
  (** [Src] is used to typeset code snippets. The integer is the line number in the source file. *)
| Custom of string * string * t list
  (** Custom block of the form
      #+begin_name opts
      DATA
      #+end *)
| Latex_Environment of string * string * string list
  (** Latex environment. Of the form
      {v \begin{foo}
      bar
      \end{foo} v}
  *)
| Drawer of string * t list
  (** A drawer *)
| Property_Drawer of (string * string) list
  (** A property drawer *)
| Footnote_Definition of string * Inline.t list
  (** The definition of a footnote : name and contents *)
| Horizontal_Rule
  (** Horizontal rule *)
| Table of table
(** A block *)

(** {1 Mapper and folders} *)
(** See {!Document} for a description *)

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
class virtual ['a, 'b] bottomUpWithArg : object
  inherit ['a, 'b] Inline.bottomUpWithArg
  method block : 'b -> t -> 'a
  method blocks : 'b -> t list -> 'a
  method list_item : 'b -> list_item -> 'a
end
