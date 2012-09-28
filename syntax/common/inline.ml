
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
  options: string option;
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
  | Plain of string

(* chars because occurences make identation screw up *)
let obracket, cbracket = ('[', ']')
let ochev,    cchev    = ('<', '>')
let oparen,   cparen   = ('(', ')')
let obrace,   cbrace   = ('{', '}')
(** {1 Parsers} *)

type parser = (string -> t list) -> Substring.t -> (t list * Substring.t) option
let rec run_parsers parsers string =
  let substring = Substring.all string in
  let myself = run_parsers parsers in
  let push acc start stop = 
    if stop = start then
      acc
    else
      let s = unescape (Substring.substring string start (stop - start)) in
      Plain s :: acc
  in
  let rec skip_until ?(k = 0) p s =
    if k >= Substring.length s || p (Substring.get s k) then k
    else skip_until ~k:(k+1) p s
  in
  let rec skip_word s = 
    if Substring.length s > 0 && Substring.get s 0 = '\\' then 
      2+skip_word (Substring.triml 2 s)
    else
      skip_until (fun c -> not (Char.is_whitespace c && Char.is_newline c)) s
        ~k: (skip_until ~k:1 (fun c -> not (Char.is_latin1 c || Char.is_digit c)) s) 
  in
  let rec aux start acc substring = 
    let (_, current, _) = Substring.base substring in
    if Substring.is_empty substring then 
      List.rev (push acc start current)
    else
      let rec try_parsers = function
        | [] -> 
            let lg = skip_word substring in
            aux start acc
              (Substring.triml lg substring)
        | t :: q -> try 
                      match t myself substring with
                        | None -> try_parsers q
                        | Some (r, cont) ->
                          let _, new_off, _ = Substring.base cont in
                            aux new_off (r @ push acc start current) cont
          with _ -> try_parsers q
      in try_parsers parsers
  in aux 0 [] substring

(* The table of delimiters used to parse inline contents *)
 let delim_table = [('[', (']', false)); ('<', ('>', false));
                   ('{', ('}', false)); ('(', (')', false));
                   ('*', ('*', false)); ('_', ('_', true));
                   ('~', ('~', false)); ('$', ('$', true)); 
                   ('=', ('=', true)); ('/', ('/', false))]
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

let inside_force delim rest = 
  match inside delim rest with
    | Some c, d -> c, d
    | _ -> failwith ""

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
let id = ref 0
let footnote_reference_parser parse rest = 
  let contents, rest = inside obracket rest in
  match contents with
    | None -> None
    | Some contents ->
      if (try int_of_string contents; true with _ -> false) then
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
  let link, rest = until_space (fun x -> false) rest in
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
  let contents, rest = inside_force '=' rest in
  Some ([Verbatim (unescape ~l:['='] (BatSubstring.all contents))], rest)
    
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


let parse = run_parsers
  [emphasis_parser; entity_parser; export_snippet_parser;
   footnote_reference_parser; inline_call_parser;
   target_parser; inline_source_block_parser; latex_fragment_parser;
   break_line_parser; link_parser; link_inline_parser;
   macro_parser; radio_target_parser; verbatim_parser; subscript_parser;
   superscript_parser; statistics_cookie_parser; 
   timestamp_parser
  ]
  
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
  method inlines v = List.map (self#inline v)
end

class ['a] folder = object(self)
  method inline (v:'a) = function
    | Emphasis (a, b) ->
      self#inlines v b
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
  method inlines = self#combine -| List.map self#inline
end

class virtual ['a, 'b] bottomUpWithArg = object(self)
  method virtual bot : 'a
  method virtual combine : 'a list -> 'a
  method inline (arg: 'b) = function
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
  method inlines arg = self#combine -| List.map (self#inline arg)
end
let string_of_url = function
  | File s | Search s -> s
  | Complex {link; protocol} -> protocol ^ ":" ^ link
let rec ascii = function
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
