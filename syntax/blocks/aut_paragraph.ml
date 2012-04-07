(** Automata for paragraphs *)
(*
This is the first and the simplest automaton. A paragraph is simply a bunch of
lines that end with an empty line.
*)
open Automaton
type state = string list
(* The state : the lines seen so far *)

(* When interrupting this parser, we just parse the inline contents, if the list of
lines is non-empty: *)
let interrupt lines _ = 
  if lines = [] then []
  else
    let lines = String.concat "\n" (List.rev lines) in
    let content = Inline.parse lines in
    if content <> [] then
      [Block.Paragraph content]
    else
      []
        
(* To parse a string, we just check if it's empty. If so we are done. If not, we
   are partially done (can be interrupted). *)
let parse_line lines { line } = 
  if line = "" then Done (interrupt lines (), true)
  else Partial (line :: lines)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line } = Some [line]

let priority = 0
