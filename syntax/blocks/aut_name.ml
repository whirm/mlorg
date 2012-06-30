(** Automata for names *)

open Batteries
open Block
open Automaton

type state = string

let interrupt s _ = [Name s]
let parse_line s _ = Done (interrupt s (), false)
let is_start { line } = try
  Scanf.sscanf line "#+name: %[^\n]"
    (fun s -> Some s)
  with _ -> None
let priority = 12
