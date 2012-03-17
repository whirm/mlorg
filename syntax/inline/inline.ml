
open Prelude
open Batteries
(** {1 Type definition} *)
type emphasis = [`Bold | `Italic | `Underline] * t list
and entity = Entity.t
and export_snippet = string * string
and footnote_reference = {
  name : string option;
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
  options: string option;
  code: string;
}
and t = 
  | Emphasis of emphasis 
  | Entity of entity
  | Export_Snippet of export_snippet
  | Footnote_Reference of footnote_reference
  | Inline_Call of inline_call
  | Inline_Source_Block of inline_source_block
  | Plain of string

(* chars because occurences make identation screw up *)
let obracket, cbracket = ('[', ']')
let oparen,   cparen   = ('(', ')')
let obrace,   cbrace   = ('{', '}')
(** {1 Parsers} *)

type parser = (string -> t list) -> Substring.t -> (t list * Substring.t) option
let rec run_parsers parsers string =
  let substring = Substring.all string in
  let myself = run_parsers parsers in
  let push acc chars = 
    if chars = [] then acc
    else
      let s = unescape (Substring.all (String.of_list (List.rev chars))) in
      Plain s :: acc
  in
  let rec aux chars acc substring = 
    if Substring.is_empty substring then 
      List.rev (push acc chars)
    else
      let rec try_parsers = function
        | [] -> 
          aux (Substring.get substring 0 :: chars)
            acc
            (Substring.triml 1 substring)
        | t :: q -> try match t myself substring with
            | None -> try_parsers q
            | Some (r, cont) ->
              aux [] (r @ push acc chars) cont
          with _ -> try_parsers q
      in try_parsers parsers
  in aux [] [] substring

(* The table of delimiters used to parse inline contents *)
let delim_table = [('[', (']', false)); ('<', ('>', false));
                   ('{', ('}', false)); ('(', (')', false));
                   ('*', ('*', true)); ('_', ('_', true));
                   ('~', ('~', true)); ('$', ('$', true)); 
                   ('=', ('=', true)); ('/', ('/', true))]
module D = Delimiters.Make (struct let table = delim_table end)
open Substring
module String = Substring

(** {2 Interesting routines} *)
open Option
let inside delim rest = 
  if String.is_empty rest then None, rest
  else match D.enclosing_delimiter rest delim with
    | Some (c, d) -> Some c, d
    | None -> None, rest

let see s rest = 
  let (before, after) = Substring.split_at (Batteries.String.length s) rest in
  if Substring.to_string before = s then after
  else raise (Failure "")
let skip ?(n = 1) rest = triml n rest
let until pred rest = 
  splitl (pred |- not) rest |> fun (x, y) -> Substring.to_string x, y
let one_of l c = List.mem c l
let ( ||| ) f g = fun x -> f x || g x
let until_space f = until (Char.is_whitespace ||| f)
(** {2 Emphasis} *)
let emphasis_parser parse sub = 
  let delims = ['*', `Bold; '_', `Underline; '/', `Italic] in
  if List.mem_assoc sub.[0] delims then
    match D.enclosing_delimiter sub sub.[0] with
      | Some (s, rest) ->
        Some ([Emphasis (List.assoc sub.[0] delims,
                         parse s)], rest)
      | None -> None
  else None

(** {2 Entity parser} *)
let entity_parser _ rest = 
  let rest = see "\\" rest in
  let name, rest = until_space (fun _ -> false) rest in
  Some ([Entity (Entity.find name)], rest)

(** {2 Export snippet parser} *)
let export_snippet_parser _ rest = 
  let rest = see "@" rest in
  let name, rest = until (Char.is_letter |- not) rest in
  let contents, rest = inside obrace rest in
  match contents with
    | None -> None
    | Some s -> Some ([Export_Snippet (name, unescape (all s))], rest)

(** {2 Footnote reference parser} *)
let footnote_reference_parser parse rest = 
  let contents, rest = inside obracket rest in
  match contents with
    | None -> None
    | Some contents ->
      if (try int_of_string contents; true with _ -> false) then
        Some ([Footnote_Reference { name = Some contents; definition = None }], 
              rest)
      else
        let contents = see "fn:" (Substring.all contents) in
        let name, contents = until ((=) ':') contents in
        let contents = skip contents in
        let name = if name = "" then None else Some name in
        let definition = 
          if String.is_empty contents then None 
          else Some (parse (String.to_string contents))
        in
        Some ([Footnote_Reference{ name; definition}], rest)

(** {2 Inline call parser} *)
let inline_call_parser _ rest = 
  let rest = see "call_" rest in
  let program, rest = until_space (one_of [oparen; obracket]) rest in
  let inside_headers, rest = inside obracket rest in
  let arguments, rest = inside oparen rest in
  let end_headers, rest = inside obracket rest in
  Some ([
    Inline_Call { program; end_headers; inside_headers;
                  arguments = map_default Config.parse_comma [] arguments }],
        rest)
(** {2 Inline Source Block parser} *)
let inline_source_block_parser _ rest = 
  let rest = see "src_" rest in
  let language, rest = until_space (one_of [obracket; obrace]) rest in
  let options, rest = inside obracket rest in
  let code, rest = inside obrace rest in
  match code with
    | None -> None
    | Some code ->
      Some ([Inline_Source_Block {language; options; code}], rest)
let parse = run_parsers
  [emphasis_parser; entity_parser; export_snippet_parser;
   footnote_reference_parser; inline_call_parser;
   inline_source_block_parser
  ]
  
