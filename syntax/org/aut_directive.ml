(** Automata for directives *)
open Block
open Org_automaton
type state = string * string
(* org mode keywords *)
let affiliated_keywords = 
  ["ATTR_ASCII"; "ATTR_DOCBOOK"; "ATTR_HTML"; "ATTR_LATEX"; "ATTR_ODT"; "CAPTION";
   "DATA"; "HEADER"; "HEADERS"; "LABEL"; "NAME"; "PLOT"; "RESNAME"; "RESULT"; "RESULTS";
   "SOURCE"; "SRCNAME"; "TBLNAME"]
let keywords_translation = 
  [("DATA", "NAME"); ("LABEL", "NAME"); ("RESNAME", "NAME");
    ("SOURCE", "NAME"); ("SRCNAME", "NAME"); ("TBLNAME", "NAME");
    ("RESULT", "RESULTS"); ("HEADERS", "HEADER")]

let translate x = try List.assoc x keywords_translation with Not_found -> x
let interrupt ctx (a, b) _ =
  ctx, 
  if List.mem (String.uppercase a) affiliated_keywords then
    [With_Keywords ([translate (String.uppercase a), b], Paragraph [])] (* little hack *)
  else
    [Directive (a, b)]
  
      
(* To parse a string, we just check if it's empty. If so we are done. If not, we
   are partially done (can be interrupted). *)
let parse_line st { context } = 
  let ctx, block = interrupt context st () in
  ctx, Done (block, false)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line; context } = 
  try
    Scanf.sscanf line "#+%[^:]: %[^\n]" 
      (fun key value -> Some (context, (key, value)))
  with _ -> None

let priority = 10
