(** Automata for directives *)
open Block
open Automaton
type state = string * string
(* org mode keywords *)
let affiliated_keywords = 
  ["ATTR_ASCII"; "ATTR_DOCBOOK"; "ATTR_HTML"; "ATTR_LATEX"; "ATTR_ODT"; "CAPTION";
   "DATA"; "HEADER"; "HEADERS"; "LABEL"; "NAME"; "PLOT"; "RESNAME"; "RESULT"; "RESULTS";
   "SOURCE"; "SRCNAME"; "TBLNAME"]
let interrupt (a, b) _ =
  if List.mem (String.uppercase a) affiliated_keywords then
    [With_Keywords ([String.uppercase a, b], Paragraph [])] (* little hack *)
  else
    [Directive (a, b)]
  
      
(* To parse a string, we just check if it's empty. If so we are done. If not, we
   are partially done (can be interrupted). *)
let parse_line st { line } = 
  Done (interrupt st (), false)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line } = 
  try
    Scanf.sscanf line "#+%[^:]: %[^\n]" (fun key value -> Some (key, value))
  with _ -> None

let priority = 10
