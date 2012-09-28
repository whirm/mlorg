(* parse verbatim contents *)
open Batteries
open Org_automaton
type state = int * string list
let interrupt context (n, lines) _ = context, [Block.Example (n, List.rev lines)]
        
let cut s = 
  if s = ":" then Some ""
  else if String.length s > 2 && s.[0] = ':' && s.[1] = ' ' then
    Some (String.sub s 2 (String.length s - 2))
  else
    None
let parse_line (n, lines) { line; context } = match cut line with
  | Some s -> context, Next (n, s :: lines)
  | None -> 
    let context, block = interrupt context (n, lines) () in
    context, Done (block, false)

let is_start { context; line } = 
  Option.map (fun s -> context, (context.Org_context.number, [s])) (cut line)
let priority = 5
