(** Automata for math *)

open Batteries
open Block
open Org_automaton
type state = (string list * bool) * int
(* The state : the lines seen so far, are we done, and the line number *)

let interrupt context ((lines, b), numb) _ = 
  if not b then Log.warning "$$ not terminated (started at line %d)" numb;
  context, [Math (String.concat "\n" (List.rev lines))]
        
let handle_line (lines, _) line =
  let line = String.trim line in
  if String.ends_with line "$$" then
    String.sub line 0 (String.length line - 2) :: lines, true
  else
    line :: lines, false

let parse_line ((lines, b), num) { line; parse; context } = 
  if b then 
    let context, blocks = interrupt context ((lines, b), num) () in
    context, Done (blocks, false)
  else
    context, Next (handle_line (lines, b) line, num)


let is_start { line; context; } = 
  try Scanf.sscanf line " $$%[^\n]"
        (fun l -> Some (context, (handle_line ([], false) l, context.Org_context.number)))
  with _ -> None

let priority = 10
