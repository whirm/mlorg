
open Prelude
open Batteries
open Inline
module Substring = BatSubstring
module String = Substring
open Substring
open Inline_combinators
(* chars because occurences make identation screw up *)
let obracket, cbracket = ('[', ']')
let ochev,    cchev    = ('<', '>')
let oparen,   cparen   = ('(', ')')
let obrace,   cbrace   = ('{', '}')
  
let delim_table = [('[', (']', false)); ('<', ('>', false));
                   ('{', ('}', false)); ('(', (')', false));
                   ('*', ('*', false)); ('_', ('_', true));
                   ('~', ('~', false)); ('$', ('$', true)); 
                   ('=', ('=', true)); ('/', ('/', false))]


let inside = inside delim_table
let inside_force = inside_force delim_table

open Option

(** {2 Emphasis} *)
let emphasis_parser parse sub = 
  let delims = ['*', `Bold; '_', `Underline; '/', `Italic] in
  if List.mem_assoc sub.[0] delims then
    let s, rest = inside_force sub.[0] sub in
    Some ([Emphasis (List.assoc sub.[0] delims, parse s)], rest)
  else None

(** {2 Entity parser} *)
let entity_parser _ rest = 
  let rest = see "\\" rest in
  let name, rest = until_space (fun _ -> false) rest in
  Some ([Entity (Entity.find name)], rest)

(** {2 Export snippet parser} *)
let export_snippet_parser _ rest = 
  let rest = see "@" rest in
  let name, rest = until (Char.is_letter %> not) rest in
  let contents, rest = inside obrace rest in
  match contents with
    | None -> None
    | Some s -> Some ([Export_Snippet (name, unescape (all s))], rest)

(** {2 Footnote reference parser} *)
let id = ref 0
let footnote_reference_parser parse rest = 
  let contents, rest = inside obracket rest in
  match contents with
    | None -> None
    | Some contents ->
      if (try ignore (int_of_string contents); true with _ -> false) then
        Some ([Footnote_Reference { name = contents; definition = None }], 
              rest)
      else
        let contents = see "fn:" (Substring.all contents) in
        let name, contents = until ((=) ':') contents in
        let contents = skip contents in
        let name = if name = "" then
          (incr id;  "_anon_" ^ string_of_int !id)
          else
            name
        in
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

(** {2 Latex Fragment parser} *)
let latex_fragment_parser _ rest = 
  let data, rest = inside '$' rest in
  match data with
    | Some s -> Some ([Latex_Fragment (Math s)], rest)
    | None -> 
      let rest = see "\\" rest in
      let command, rest = until_space ((=) obrace) rest in
      let options, rest = inside_force obrace rest in
      Some ([Latex_Fragment (Command (command, options))], rest)


(** {2 Break Line parser} *)
let break_line_parser _ rest = 
  let rest = see "\\\\" rest in
  let rest = if BatSubstring.is_empty rest then rest else see "\n" rest in
  Some ([Break_Line], rest)

(** {2 Link parser} *)
let link_parser parse rest =
  let contents, rest = inside_force obracket rest in
  let first, second = inside_force obracket (all contents) in
  let second, _ = inside obracket second in
  let descr, url = match second with
    | None -> first, Search first
    | Some descr ->
        if (all first).[0] = '/' || (all first).[0] = '.' then
          descr, File first
        else try Scanf.sscanf first "%[^:]:%[^\n]" 
                   (fun protocol link -> descr, Complex {protocol; link})
          with _ -> descr, Search first
  in
  Some ([Link {label = parse descr; url}], rest)

let link_inline_parser _ rest = 
  let protocol, rest = until_space ((=) ':') rest in
  let rest = see "://" rest in
  let pred = no_delim_capture delim_table in
  let link, rest = until_space (fun c -> not (pred c)) rest in
  Some ([Link {label = [Plain (protocol ^ "://" ^ link)];
               url = Complex { protocol; link = "//"^link }}],
        rest)
  
(** {2 Macro parser} *)
let macro_parser _ rest = 
  let trim = Batteries.String.trim and nsplit = Batteries.String.nsplit in
  let rest = see "{{" rest in
  let contents, rest = inside_force obrace rest in
  let rest = see "}}" rest in
  let contents = all contents in
  let name, contents = until ((=) oparen) contents in
  let name = trim name in
  let contents = see "(" contents in
  let arguments, contents = until ((=) cparen) contents in
  Some ([Macro (name, List.map trim (nsplit arguments ","))],
        rest)

(** {2 Radio Target parser} *)
let radio_target_parser _ rest = 
  let rest = see "<<" rest in
  let contents, rest = inside_force ochev rest in
  let rest = see ">>" rest in
  Some ([Radio_Target contents], rest)
let target_parser _ rest = 
  let rest = see "<" rest in
  let contents, rest = inside_force ochev rest in
  let rest = see ">" rest in
  Some ([Target contents], rest)

(** {2 Verbatim parser} *)
let verbatim_parser _ rest =
  if not (List.mem rest.[0] ['='; '~']) then assert false;
  let c = rest.[0] in
  let contents, rest = inside_force c rest in
  Some ([Verbatim (unescape ~l:[c] (BatSubstring.all contents))], rest)
    
(** {2 Subscript and Superscript parser} *)
let subscript_parser, superscript_parser = 
  let gen c f parse rest = 
    let rest = see c rest in
    let contents, rest = inside_force obrace rest in
    Some ([f (parse contents)], rest)
  in gen "_" (fun x -> Subscript x),
     gen "^" (fun x -> Superscript x)


(** {2 Statistics cookie parser} *)
let statistics_cookie_parser _ rest =
  let contents, rest = inside_force obracket rest in
  let cookie = 
    (try Scanf.sscanf contents "%d/%d" (fun n n' -> Absolute (n, n'))
     with _ -> Scanf.sscanf contents "%d%%" (fun n -> Percent n))
  in
  Some ([Cookie cookie], rest)

(** {2 Timestamp parser} *)
let timestamp_parser _ rest = 
  let handle f rest = 
    let rest = if Substring.length rest = 0 then rest
      else if Substring.get rest 0 = ' ' then Substring.triml 1 rest
      else rest
    in
    match Timestamp.parse_substring rest with
      | Some (a, rest) -> f a, rest
      | None -> raise (Failure "")
  in
  let try_range f g rest = match Timestamp.parse_range_substring rest with
    | Some (a, rest) -> f a, rest
    | None ->
      match Timestamp.parse_substring rest with
        | Some (a, rest) -> g a, rest
        | None -> raise (Failure "")
  in
  let timestamp, rest = match until_space (fun _ -> false) rest with
    | "SCHEDULED:", rest -> handle (fun x -> Scheduled x) rest
    | "CLOSED:", rest -> handle (fun x -> Closed x) rest
    | "DEADLINE:", rest -> handle (fun x -> Deadline x) rest
    | "CLOCK:", rest -> 
        let a, rest = try_range (fun x -> Clock (Stopped x))
          (fun x -> Clock (Started x)) (Substring.triml 1 rest) in
        a, Substring.dropl ((<>) '\n') rest

    | _ -> try_range (fun x -> Range x) (fun x -> Date x) rest
  in
  Some ([Timestamp timestamp], Substring.trim rest)


let parse = run_parsers (fun s -> Plain s)
  [emphasis_parser; entity_parser; export_snippet_parser;
   footnote_reference_parser; inline_call_parser;
   target_parser; inline_source_block_parser; latex_fragment_parser;
   break_line_parser; link_parser; link_inline_parser;
   macro_parser; radio_target_parser; verbatim_parser; subscript_parser;
   superscript_parser; statistics_cookie_parser; 
   timestamp_parser
  ]
  
