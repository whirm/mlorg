(** Inline contents: type definition and output. *)


(** {1 Description} *)

(** This file deal with parsing inline contents. It is called /objects= in =org-element=.
This is the list of possible elements:
- emphasis (of several kind : italic, underline, bold, ...)
- entities : html or latex entities embded in org files, such as \alpha, &aacute, ...
- export-snippet [\@type\{value\}] : this construction is to insert
  raw data for a specific exporter:for instance [\@html\{\<p\>Paragraph\</p\>\}]
- footnote reference : [[fn:foo]] for instance
- inline babel call : make a call to a babel source block. More details [[http://orgmode.org/manual/Evaluating-code-blocks.html][here]].
- inline source block : [src_lang[arg]\{contents\}]
- latex fragment : =$foo$=
- line break : =\\\\= at the end of a line
- link : =[[descr][target]]= 
- macro : text substitution [{{{name(arg1, arg2)}}}]. Documentation [[http://orgmode.org/manual/Macro-replacement.html][here]].
- radio target : special links  [<<<target>>>]
- statistics-cookie : statistics about the percentage of done children
- verbatim : [=plaincode=]
- superscript/subscript : [^\{foo\}] and [_\{foo\}]
- targets : special links (?)
- timestamp : scheduled/deadline or plain/inactive timestamp

The file is organized as follows :
- definition of types corresponding to the category below
- definition of what a parser is (see below for more information)
- definition of parsers for each syntaxic categories
- glue that does the job
*)

(** {1 Type definitions} *)

(**
This section follows the following convention :
- a type for each category
- all types are mutually recursive, if not needed.

Indeed for some categories (link, emphasis, ...) we need to embded inline content.
*)

(** {2 Emphasis} *)
(** org supports three types of emphasis :
- bold (with stars : [*foo*])
- italic (with slashes : [/foo/])
- underline (with underscores : [_foo_])
*)
type emphasis = [`Bold | `Italic | `Underline] * t list

(** {2 Entities} *)
(** Entity are defined in the module {!Entity}. *)

and entity = Entity.t

(** {2 Export snippett} *)
(** An export snippet is given by a couple [(language, text)].*)

and export_snippet = string * string

(** {2 Footnote reference} *)
(** A footnote reference contains:
    - a name (optional)
    - a definition (optional) *)

and footnote_reference = {
  name : string option;
  definition : t list option;
}

(** {2 Inline call} *)
(** An inline call consists in
    - The name of the block to call
    - The arguments
    - Inside header arguments
    - End header arguments.
    Cf org's doc for more information
*)

and inline_call = {
  program : string;
  arguments : (string * string) list;
  inside_headers : string option;
  end_headers : string option;
}

(** {2 Inline source block} *)

and inline_source_block = {
  language: string;
  options: string option;
  code: string;
}

(** {2 Latex fragments} *)

and latex_fragment = 
  | Math of string
  | Command of string * string
(** A latex fragment is either math contents [$foo$] or commands [$\ref{x}$] *)

(** {2 Links} *)
(** Links are composed of two parts : an url and a label.
    An url may be pointed to a file, to a search or to an actual url *)
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

(** {2 Cookies} *)

and stats_cookie = 
  | Percent of int
  | Absolute of int * int (** current, max *)
(** Cookies are a way to indicate the progress of a task. 
    They can be of two form : percentage or absolute value *)


(** {2 Timestamps} *)

and clock_item = 
  | Started of Timestamp.t
  | Stopped of Timestamp.range
(** A clock item-- either stopped or 
    started *)

and timestamp = 
  | Scheduled of Timestamp.t
  | Deadline of Timestamp.t
  | Date of Timestamp.t
  | Closed of Timestamp.t
  | Clock of clock_item
  | Range of Timestamp.range

(** {2 The type of inline contents} *)
(** The final type for {!Inline.t} is as follows: *)
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
  | Plain of string

(** {1 Parsers} *)
(** This section deals with defining what a parser for a construction is. *)

type parser = (string -> t list) -> BatSubstring.t -> (t list * BatSubstring.t) option
  (** A parser simply a function taking a [BatSubstring.t] and returning an option of
      [t list]. The parser takes an extra parameter, a function to parse a
      string and return a list of {!Inline.t}. The function may as well throw an
      exception.
  *)

val run_parsers : parser list -> string -> t list
(** Run a list of parsers over a string and returned the generated contents.
    Note that the order matter : first parsers have higher priority. *)

val parse : string -> t list
(** Parse a string *)


(** {1 Mappers and folders} *)
(** In this section we define mapper and folders overline *)
class ['a] mapper :
object
  method inline : 'a -> t -> t
  method inlines : 'a -> t list -> t list
end

class ['a] folder :
object
  method inline : 'a -> t -> 'a
  method inlines : 'a -> t list -> 'a
end

class virtual ['a] bottomUp :
object
  method virtual bot : 'a
  (** The default value for leaf *)
  method virtual combine : 'a list -> 'a
  (** Combining a list of result *)
  method inline : t -> 'a
  (** Traverse a single element *)
  method inlines : t list -> 'a
(** Traverse a list of elements and combine their results *)
end
(** Implements a bottom up traversal of the tree
    where contents is created from the leaf and propagated upward.
    This is very useful for exporters *)

(** {1 Useful tools about inline} *)
val ascii : t -> string
val asciis : t list -> string
(** Convert inline contents to plain ascii *)

val string_of_url : url -> string
(** Convert an url to a string *)
