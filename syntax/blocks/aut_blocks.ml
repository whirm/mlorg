(** Automata for blocks *)
open Batteries
open Block
open Automaton
type state = int * string list * string * string
(* The state : the linenumber, the lines seen so far, the name and options *)

let interrupt (number, lines, name, opts) parse = 
  let lines = List.rev lines in
  [match name with
    | "example" -> Example (number, lines)
    | "src" -> Src (number, opts, lines)
    | "quote" -> Quote (parse (List.enum lines))
    | _ ->
      Custom (name, opts, parse (List.enum lines))]
        
let parse_line (number, lines, name, opts) { line; parse } = 
  let re = Str.regexp "#\\+\\(end\\|END\\)_\\([^ ]+\\)" in
  if Str.string_match re line 0 then
    if Str.matched_group 2 line <> name then 
      Next (number, line :: lines, name, opts)
    else 
      Done (interrupt (number, lines, name, opts) parse, true)
  else 
    Next (number, line :: lines, name, opts)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line; number } = 
  let re = Str.regexp "#\\+\\(begin\\|BEGIN\\)_\\([^ ]+\\)\\( +\\(.+\\)\\)?" in
  if Str.string_match re line 0 then
    let name = Str.matched_group 2 line 
    and options = try Str.matched_group 4 line with Not_found -> "" in
    Some (number, [], name, String.trim options)
  else
    None

let priority = 10
