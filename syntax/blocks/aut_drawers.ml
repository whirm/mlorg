(* Automata for properties *)
(* Lines seen so far and draw name *)
open Automaton
open Block
open Batteries
type state = string list * string

let is_start { line; context } = 
  try
    Scanf.sscanf (String.trim line) ":%[^:]:%!" 
      (fun name -> Some (context, ([], name)))
  with _ -> None


let parse_properties = 
  List.fold_left (fun acc line ->
    try Scanf.sscanf (String.trim line) ":%[^:]: %[^\n]"
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

let interrupt context (lines, name) parse = 
  let lines = List.rev lines in
  if name = "properties" || name = "PROPERTIES" then
    context, [Property_Drawer (parse_properties lines)]
  else
    let context, blocks = parse context (List.enum lines) in
    context, [Drawer (name, blocks)]

let parse_line (lines, name) { line; context; parse } = 
  try Scanf.sscanf (String.trim line) " :%[^:]:" (fun name' ->
    if name' = "end" || name' = "END" then
      let context, block = interrupt context (lines, name) parse in
      context, Done (block, true)
    else
      context, Next (line :: lines, name))
  with _ -> context, Next (line :: lines, name)

let priority = 10
