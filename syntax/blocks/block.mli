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
and list = list_item list

(** {2 Blocks} *)
and t = 
  | Paragraph of Inline.t list
  | Heading of heading
  | List of list


