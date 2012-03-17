
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
and t = 
  | Emphasis of emphasis 
  | Entity of entity
  | Export_Snippet of export_snippet
  | Footnote_Reference of footnote_reference
  | Plain of string


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
                   ('{', ('}', false));
                   ('*', ('*', true)); ('_', ('_', true));
                   ('~', ('~', true)); ('$', ('$', true)); 
                   ('=', ('=', true)); ('/', ('/', true))]
module D = Delimiters.Make (struct let table = delim_table end)
open Substring
module String = Substring

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
let entity_parser _ s = 
  if s.[0] = '\\' then
    let name, rest = splitl 
      (fun c -> Char.is_letter c || Char.is_digit c) (triml 1 s)
    in
    Some ([Entity (Entity.find (to_string name))], rest)
  else
    None

(** {2 Export snippet parser} *)
let export_snippet_parser _ s = 
  if s.[0] = '@' then
    let name, rest = splitl Char.is_letter (triml 1 s) in
    (if rest.[0] = '{' then
        (match D.enclosing_delimiter rest '{' with
          | Some (s, rest) -> 
            Some ([Export_Snippet (to_string name, unescape (all s))], rest)
          | None -> None)
      else
        None)
  else
    None

(** {2 Footnote reference parser} *)
let footnote_reference_parser parse s = 
  if s.[0] <> '[' then None
  else match D.enclosing_delimiter s '[' with
    | None -> None
    | Some (s, rest) -> 
      let data = 
        try
          let _ = int_of_string s in
          { name = Some s;
            definition = None }
        with _ -> 
          let parse' l = parse (Batteries.String.concat ":" l) in
          match Batteries.String.nsplit s ":" with
          | "fn" :: "" :: def -> { name = None; definition = Some (parse' def) }
          | ["fn"; name] -> { name = Some name; definition = None }
          | "fn" :: name :: def ->
            { name = Some name; definition = Some (parse' def) }
      in Some ([Footnote_Reference data], rest)

let parse = run_parsers
  [emphasis_parser; entity_parser; export_snippet_parser;
   footnote_reference_parser
  ]
  
