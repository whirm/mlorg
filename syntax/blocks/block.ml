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

