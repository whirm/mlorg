
open Prelude
open Batteries

module Substring = BatSubstring
(** {1 Type definition} *)
type emphasis = [`Bold | `Italic | `Underline] * t list
and entity = Entity.t
and export_snippet = string * string
and footnote_reference = {
  name : string;
  definition : t list option;
}
and inline_call = {
  program : string;
  arguments : (string * string) list;
  inside_headers : string option;
  end_headers : string option;
}
and inline_source_block = {
  language: string;
  options: Hd_arguments.t;
  code: string;
}
and url = 
  | File of string
  | Search of string
  | Complex of complex
and complex = {
  protocol: string;
  link: string
}
    
and link = {
  url: url;
  label: t list;
}
and stats_cookie = 
  | Percent of int
  | Absolute of int * int (** current, max *)
and clock_item = 
  | Started of Timestamp.t
  | Stopped of Timestamp.range
and timestamp = 
  | Scheduled of Timestamp.t
  | Deadline of Timestamp.t
  | Date of Timestamp.t
  | Closed of Timestamp.t
  | Clock of clock_item
  | Range of Timestamp.range
and latex_fragment = 
  | Math of string
  | Command of string * string
and t = 
  | Emphasis of emphasis 
  | Entity of entity
  | Export_Snippet of export_snippet
  | Footnote_Reference of footnote_reference
  | Inline_Call of inline_call
  | Inline_Source_Block of inline_source_block
  | Latex_Fragment of latex_fragment
  | Break_Line
  | Link of link
  | Macro of string * string list
  | Radio_Target of string
  | Target of string
  | Subscript of t list
  | Superscript of t list
  | Verbatim of string
  | Cookie of stats_cookie
  | Timestamp of timestamp
  | List of t list
  | Plain of string

(* *)

class ['a] mapper = object(self)
  method inline (v:'a) = function
    | Emphasis (a, b) ->
      Emphasis (a, self#inlines v b)
    | Footnote_Reference ref ->
      Footnote_Reference { ref with definition = 
          Option.map (self#inlines v) ref.definition }
    | Link l ->
      Link { l with label = self#inlines v l.label }
    | Subscript t -> Subscript (self#inlines v t)
    | Superscript t -> Superscript (self#inlines v t)
    | (Macro _
    | Radio_Target _
    | Verbatim _
    | Cookie _
    | Timestamp _
    | Plain _
    | Inline_Call _
    | Inline_Source_Block _
    | Latex_Fragment _
    | Target _
    | Break_Line 
    | Export_Snippet _ 
    | Entity _) as x -> x
    | List l -> List (self#inlines v l)
  method inlines v = List.map (self#inline v)
end

class ['a] folder = object(self)
  method inline (v:'a) = function
    | Emphasis (a, b) ->
      self#inlines v b
    | List l -> self#inlines v l
    | Footnote_Reference ref ->
      Option.map_default (self#inlines v) v ref.definition
    | Link l ->
      self#inlines v l.label
    | Subscript t -> self#inlines v t
    | Superscript t -> self#inlines v t
    | Macro _
    | Radio_Target _
    | Verbatim _
    | Cookie _
    | Timestamp _
    | Plain _
    | Inline_Call _
    | Inline_Source_Block _
    | Target _
    | Latex_Fragment _
    | Break_Line 
    | Export_Snippet _ 
    | Entity _ -> v
  method inlines v = List.fold_left self#inline v
end


class virtual ['a] bottomUp = object(self)
  method virtual bot : 'a
  method virtual combine : 'a list -> 'a
  method inline = function
    | List l -> self#inlines l
    | Emphasis (a, b) ->
        self#inlines b
    | Footnote_Reference ref ->
        Option.map_default self#inlines self#bot ref.definition
    | Link l ->
      self#inlines l.label
    | Subscript t -> self#inlines t
    | Superscript t -> self#inlines t
    | Macro _
    | Radio_Target _
    | Verbatim _
    | Cookie _
    | Timestamp _
    | Plain _
    | Inline_Call _
    | Inline_Source_Block _
    | Latex_Fragment _
    | Break_Line 
    | Target _
    | Export_Snippet _ 
    | Entity _ -> self#bot
  method inlines = self#combine % List.map self#inline
end

class virtual ['a, 'b] bottomUpWithArg = object(self)
  method virtual bot : 'a
  method virtual combine : 'a list -> 'a
  method inline (arg: 'b) = function
    | List l -> self#inlines arg l
    | Emphasis (a, b) ->
        self#inlines arg b
    | Footnote_Reference ref ->
        Option.map_default (self#inlines arg) self#bot ref.definition
    | Link {label=t}
    | Subscript t 
    | Superscript t -> self#inlines arg t
    | Macro _
    | Radio_Target _
    | Verbatim _
    | Cookie _
    | Timestamp _
    | Plain _
    | Inline_Call _
    | Inline_Source_Block _
    | Latex_Fragment _
    | Break_Line 
    | Target _
    | Export_Snippet _ 
    | Entity _ -> self#bot
  method inlines arg = self#combine % List.map (self#inline arg)
end
let string_of_url = function
  | File s | Search s -> s
  | Complex {link; protocol="file"} -> link
  | Complex {link; protocol} -> protocol ^ ":" ^ link
let rec ascii = function
  | List l -> asciis l
  | Footnote_Reference ref -> 
    Option.map_default asciis "" ref.definition
  | Link l -> asciis l.label
  | Emphasis (_, t)
  | Subscript t
  | Superscript t -> asciis t
  | Macro _ 
  | Radio_Target _ -> ""
  | Verbatim s -> s
  | Cookie _ -> ""
  | Timestamp _ -> ""
  | Target s -> ""
  | Latex_Fragment (Math s)
  | Plain s -> s
  | Latex_Fragment (Command (s, s')) ->
    "\\" ^ s ^ "{" ^ s' ^ "}"
  | Inline_Call _ -> ""
  | Inline_Source_Block _ -> ""
  | Break_Line -> "\n"
  | Export_Snippet _ -> ""
  | Entity e -> e.Entity.unicode
and asciis l = Batteries.String.concat "" (List.map ascii l)

