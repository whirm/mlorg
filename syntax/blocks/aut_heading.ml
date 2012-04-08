(** Automata for headings *)
open Batteries
open Prelude
open Block
open Automaton
(** To parse headings, we need to 
- we need to count the stars
- to spot an eventual marker
- to spot an eventual priority
- to spot the title
- to spot the tags

Each step will correspond to a function of type [Substring.t -> (t * Substring.t)].
*)

let count_stars s =
  let s' = Substring.dropl ((=) '*') s in
  Substring.size s - Substring.size s', Substring.trim s'


let get_marker context s = 
  let word, rest = Substring.splitl ((<>) ' ') s in
  let word = Substring.to_string word in
  if List.mem word context.Context.markers then
    Some word, rest
  else
    None, s

let get_priority s = 
  let module String = Substring in
  let word, rest = Substring.splitl ((<>) ' ') s in
  if String.size word = 4 
  && word.[0] = '[' && word.[1] = '#' && word.[3] = ']' then
    Some word.[2], rest
  else
    None, s

let get_title s = 
  let module String = Substring in
  let parse_inl = Inline.parse -| Substring.to_string in
  let title, tags = Substring.splitr (fun c -> c = ':' 
  || Char.is_letter c || Char.is_digit c) s in
  if not (Substring.is_empty tags) && 
    tags.[0] = ':' && tags.[String.size tags - 1] = ':' then
    parse_inl (Substring.trim title), Some tags
  else 
    parse_inl s, None

let get_tags s = 
  let s = Substring.triml 1 (Substring.trimr 1 s) in
  Substring.split_on_char ':' s |>
      List.map Substring.to_string 

let parse_heading context s = 
  let level, s = count_stars (Substring.of_string s) in
  let marker, s = get_marker context s in
  let priority, s = get_priority s in
  let title, tags = get_title s in
  let tags = Option.map_default get_tags [] tags in
  { title; marker; level; priority; tags }

type state = heading

let interrupt heading _ = [Heading heading]

let is_start { line; context } = 
  try
    Some (parse_heading context line)
  with _ -> None

let parse_line heading p = 
  Done (interrupt heading p, true)

let priority = 100
