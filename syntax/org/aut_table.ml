(** Automata for names *)

open Batteries
open Block
open Org_automaton

type state = string list


let is_valid line = 
  let line = String.trim line in
  if line <> "" && line.[0] = '|' && line.[String.length line - 1] = '|' then
    (if line <> "|" && line.[1] = '-' then
      Some None
    else
      Some (Some line))
  else
    None

let split_into_columns s = 
  let module D = Delimiters.Make (struct let table = ['|', ('|', false)] end) in
  D.split (BatSubstring.all s) '|' |> List.tl

let make_matrix = 
  List.rev
  %> List.map (split_into_columns %> Array.of_list)
type cell = 
  | Content of Inline.t list
  | Size of int
  | StartGroupLine
  | BeginGroup
  | EndGroup
  | Empty
let compile_matrix matrix = 
  List.map (Array.mapi (fun j s ->
    let s = String.trim s in
    if j = 0 && s = "/" then StartGroupLine
    else if s = "<" then BeginGroup
    else if s = ">" then EndGroup
    else if s = "" then Empty
    else try
           Scanf.sscanf s "<%d>" (fun n -> Size n)
      with _ -> Content (Org_inline.parse s)))
    matrix
let analyse_line (table, rows) row = 
  if Array.for_all
    (fun t -> List.mem t [StartGroupLine; Empty; BeginGroup; EndGroup])
    row
  then
    let result, _ = 
      Array.fold_lefti (fun (li, last) i -> function
        | StartGroupLine -> li, last
        | BeginGroup when last > 0 -> (* Doesn't make any sense .. ignoring *)
            Log.warning "Group annotation not making any sense: Two < in a row";
            (li, last)
        | BeginGroup -> (li, i)
        | EndGroup when last > 0 -> (last-1, i-1) :: li, -1 (* because the / will get destroyed *)
        | EndGroup -> 
            Log.warning "Group annotation not making any sense: Two > in a row";
            (li, last)
        | _ -> assert false)
        ([], -1) row
    in
    { table with groups = Some (List.rev result) }, rows
  else if Array.for_all (function Size _ | Empty -> true | _ -> false) row then
    { table with align_line = 
        Some (Array.map (function Size n -> n | _ -> 0) row) }, rows
  else
    table, row :: rows

let remove_first_col list = 
  if List.for_all (fun row -> List.mem row.(0) [StartGroupLine; Empty]) list then
    List.map (fun a -> Array.sub a 1 (Array.length a - 1)) list
  else 
    list
let build lines = 
  let matrix = make_matrix lines in
  let table, rows = List.fold_left analyse_line
    ({ format = None; align_line = None;
      groups = None; rows = [||] }, [])
    (compile_matrix matrix)
  in
  let rows = remove_first_col rows 
  |> List.map (Array.map (function Content c -> c | _ -> [Inline.Plain ""]))
  in
  { table with rows = Array.of_list rows }

let is_start { context; line } = match is_valid line with
  | None -> None
  | Some None -> Some (context, [])
  | Some (Some l) -> Some (context, [l])

let interrupt context lines r = 
  context, [Table (build lines)]

let parse_line lines ({ line; context } as r) = match is_valid line with
  | None -> 
      let pattern = "#+tblfm:" in
      if String.starts_with (String.lowercase line) pattern then
        let n = String.length pattern in
        let table = build lines in
        context, Done ([Table { table with format = Some (String.lchop ~n line) }], true)
      else
        let context, blocks = interrupt context lines r in
        context, Done (blocks, false)
  | Some None ->
      context, Next lines
  | Some (Some line) ->
      context, Next (line :: lines)

let priority = 12
