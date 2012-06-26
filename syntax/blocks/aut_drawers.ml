(* Automata for properties *)
(* Lines seen so far and draw name *)
open Automaton
open Block
open Batteries
type state = string list * string

let is_start { line } = 
  try
    Scanf.sscanf line " :%[^:]:" (fun name -> Some ([], name))
  with _ -> None


let parse_properties = 
  List.fold_left (fun acc line ->
    try Scanf.sscanf line " :%[^:]: %[^\n]"
          (fun key value -> (key, value) :: acc)
    with _ ->
      match acc with
        | [] -> Log.warning "Invalid first line in property drawer. Ignoring it";
          acc
        | (key, v) :: acc' ->
          (* Because line might be indented *)
          let line = " " ^ String.trim line in
          (key, v ^ line) :: acc')
    []

let interrupt (lines, name) _ = 
  let lines = List.rev lines in
  if name = "properties" || name = "PROPERTIES" then
    [Property_Drawer (parse_properties lines)]
  else
    [Drawer lines]

let parse_line (lines, name) { line } = 
  try Scanf.sscanf line ":%[^:]:" (fun name' ->
    if name' = "end" || name' = "END" then
      Done (interrupt (lines, name) () , true)
    else
      Next (line :: lines, name))
  with _ -> Next (line :: lines, name)

let priority = 10
