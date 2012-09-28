(** Automata for blocks *)
open Batteries
open Block
open Org_automaton
type state = int * string list * string * string
(* The state : the linenumber, the lines seen so far, the name and options *)

let interrupt ctx (number, lines, name, opts) parse = 
  let lines = List.rev lines in
  match name with
    | "example" -> ctx, [Example (number, lines)]
    | "src" -> ctx, [Src (number, opts, lines)]
    | "quote" ->
      let ctx', blocks = parse ctx (List.enum lines) in
      ctx', [Quote blocks]
    | _ ->
      let ctx', blocks = parse ctx (List.enum lines) in
      ctx', [Custom (name, opts, blocks)]
        
let parse_line (number, lines, name, opts) { line; context; parse } = 
  let re = Str.regexp "#\\+\\(end\\|END\\)_\\([^ ]+\\)" in
  if Str.string_match re line 0 then
    if Str.matched_group 2 line <> name then 
      context, Next (number, line :: lines, name, opts)
    else 
      let ctx, blocks = interrupt context (number, lines, name, opts) parse in
      ctx, Done (blocks, true)
  else 
    context, Next (number, line :: lines, name, opts)

let interrupt context ((number, _, name, _) as x) y =
  Log.warning "Unterminated block %s started at line %d" name number;
  interrupt context x y

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line; context } = 
  let re = Str.regexp "#\\+\\(begin\\|BEGIN\\)_\\([^ ]+\\)\\( +\\(.+\\)\\)?" in
  if Str.string_match re line 0 then
    let name = Str.matched_group 2 line 
    and options = try Str.matched_group 4 line with Not_found -> "" in
    Some (context, (context.Org_context.number, [], name, String.trim options))
  else
    None

let priority = 10
