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
  try Scanf.sscanf line "#+end_%s" (fun name' ->
    if name' <> name then Next (number, line :: lines, name, opts)
    else Done (interrupt (number, lines, name, opts) parse, true))
  with _ -> Next (number, line :: lines, name, opts)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line; number } = 
  try Scanf.sscanf line "#+begin_%s %[^\n]"
        (fun a b -> Some (number, [], a, b))
  with _ -> None

let priority = 10
