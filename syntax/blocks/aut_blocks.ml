(** Automata for blocks *)
open Batteries
open Block
open Automaton
type state = string list * string * string
(* The state : the lines seen so far, the name and options *)

let interrupt (lines, name, opts) parse = 
  let lines = List.rev lines in
  [match name with
    | "example" -> Example lines
    | "src" -> Src (opts, lines)
    | "quote" -> Quote (parse (List.enum lines))
    | _ ->
      Custom (name, opts, parse (List.enum lines))]
        
let parse_line (lines, name, opts) { line; number; parse } = 
  try Scanf.sscanf line "#+end_%s" (fun name' ->
    if name' <> name then Next (line :: lines, name, opts)
    else Done (interrupt (lines, name, opts) parse, true))
  with _ -> Next (line :: lines, name, opts)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line } = 
  try Scanf.sscanf line "#+begin_%s %[^\n]"
        (fun a b -> Some ([], a, b))
  with _ -> None

let priority = 10
