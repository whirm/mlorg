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
- radio target : special links
- statistics-cookie : statistics about the percentage of done children
- verbatim : [=plaincode=]
- superscript/subscript : [^\{foo\}] and [_\{foo\}]
- targets : special links [<<<target>>>]
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
(** {2 The type of inline contents} *)
(** The final type for {!Inline.t} is as follows: *)
and t = 
  | Emphasis of emphasis 
  | Entity of entity
  | Export_Snippet of export_snippet
  | Footnote_Reference of footnote_reference
  | Inline_Call of inline_call
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

