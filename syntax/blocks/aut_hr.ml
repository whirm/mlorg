(** Automata for horizontal rules *)
open Automaton
type state = unit

let interrupt context _ _ = context, [Block.Horizontal_Rule]
        
let parse_line _ { context } = context, Done ([Block.Horizontal_Rule], false)

let is_start { line; context } = 
  if String.length line >= 5 &&
    String.sub line 0 5 = "-----" then Some (context, ())
  else None
let priority = 1
