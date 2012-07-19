(** Automata for horizontal rules *)
open Automaton
type state = unit

let interrupt _ _ = [Block.Horizontal_Rule]
        
let parse_line _ _ = Done (interrupt () (), false)

let is_start { line } = 
  if String.length line >= 5 &&
    String.sub line 0 5 = "-----" then Some ()
  else None
let priority = 1
