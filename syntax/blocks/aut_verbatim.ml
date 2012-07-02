(* parse verbatim contents *)
open Batteries
open Automaton
type state = int * string list
let interrupt (n, lines) _ = [Block.Example (n, List.rev lines)]
        
let cut s = 
  if String.length s > 2 && s.[0] = ':' && s.[1] = ' ' then
    Some (String.sub s 2 (String.length s - 2))
  else
    None
let parse_line (n, lines) { line } = match cut line with
  | Some s -> Next (n, s :: lines)
  | None -> Done (interrupt (n, lines) (), false)

let is_start { line; number } = Option.map (fun s -> number, [s]) (cut line)
let priority = 5
