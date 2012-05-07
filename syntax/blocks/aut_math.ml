(** Automata for math *)

open Batteries
open Block
open Automaton
type state = string list * bool
(* The state : the lines seen so far, and are we done *)

let interrupt (lines, b) _ = 
  if not b then Log.warning "$$ not terminated";
  [Math (String.concat "\n" (List.rev lines))]
        
let handle_line (lines, _) line =
  if String.ends_with line "$$" then
    String.sub line 0 (String.length line - 2) :: lines, true
  else
    line :: lines, false

let parse_line (lines, b) { line; number; parse } = 
  if b then Done (interrupt (lines, b) (), false)
  else
    Next (handle_line (lines, b) line)


let is_start { line } = 
  try Scanf.sscanf line " $$%[^\n]"
        (fun l -> Some (handle_line ([], false) l))
  with _ -> None

let priority = 10
