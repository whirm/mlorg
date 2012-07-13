(** Automata for latex environment *)
open Batteries
open Block
open Automaton
type state = string list * string * string
(* The state : the lines seen so far, the name and options *)

let interrupt (lines, name, opts) _ = 
  let lines = List.rev lines in
  [Latex_Environment (name, opts, lines)]
        
let parse_line (lines, name, opts) { line; parse } = 
  let re = Str.regexp "\\\\end{\\([^}]+\\)}" in
  if Str.string_match re line 0 then
    if Str.matched_group 1 line <> name then 
      Next (line :: lines, name, opts)
    else 
      Done (interrupt (lines, name, opts) parse, true)
  else 
    Next (line :: lines, name, opts)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line } = 
  let re = Str.regexp "\\\\begin{\\([^}]+\\)}\\(.*\\)?$" in
  if Str.string_match re line 0 then
    let name = Str.matched_group 1 line 
    and options = try Str.matched_group 2 line with Not_found -> "" in
    Some ([], name, options)
  else
    None

let priority = 10
