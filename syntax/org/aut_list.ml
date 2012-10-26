open Prelude
open Batteries

type state = {
  items: Block.list_item list;
  (** The items gathered so far *)
  last_line_empty: bool;
  (** Was the last line empty ? *)
  current: string list;
  (** The contents of the current item *)
  number : int option;
  (** Its number *)
  format : string;
  (** Its format *)
  checkbox : bool option;
  (** Was it checked *)
  ordered : bool;
  (** Is the list ordered *)
  indent: int option;
  (** Indentation of the current item. *)
}
(** The state used for this automata *)

let compute_number ordered format default = 
  if not ordered then None
  else match Option.map Numbering.extract format with
             | Some [n] -> Some n
             | _ -> Some default
(** Compute the number for an item *)

let parse_first_line s = 
  (* Parse a number format [[@foo]] *)
  let parse_fmt s = if s.[0] = '@' then
      Some (String.sub s 1 (String.length s - 1))
    else None
  in

  (* Parses the beginning of the line,
     Returns a [int * bool] option,
     with [Some (k, b)] on success where [b] tells
     whether the list is ordered and [k] is the number of the first item, if any *)
  let next = 
    if String.length s = 0 then None
    else if String.length s > 2 && (s.[0] = '+' || s.[0] = '-') && s.[1] = ' 'then 
           Some (1, false)
    else try 
           Scanf.sscanf s "%d.%n" (fun _ k -> Some (k, true))
      with _ -> None

  in match next with
    | None -> None
    | Some (k, ordered) ->
      let module D = Delimiters.Make 
            (struct let table = ['[', (']', false)] end) in
      let open BatSubstring in
      let s = trim (triml k (all s)) in
      (* If we have the start of a list, we look for a checkbox, *)
      match D.enclosing_delimiter s '[' with
        | None -> Some (ordered, None, None, s)
        | Some (s, rest) ->
          let b = if s = " " then Some false 
            else if s = "X" then Some true
            else None
          in
          if b = None then Some (ordered, None, parse_fmt s, rest)
          else
            (* and then a format *)
            match D.enclosing_delimiter (trim rest) '[' with
              | None -> Some (ordered, b, None, rest)
              | Some (fmt, rest) -> Some (ordered, b, parse_fmt fmt, rest)
(** Parse the first line of an item *)

let is_start { Org_automaton.line; Org_automaton.context } = 
  match parse_first_line line with
    | None -> None
    | Some (ordered, checkbox, format, contents) ->
      let format' = Option.default "1" format in
      Some (context, 
            { items = []; last_line_empty = false;
              current = [BatSubstring.to_string contents]; 
              format = format'; checkbox; ordered; indent = None;
              number = compute_number ordered format 1;
            })
(** is_start basically checks if the line is a valid star for an item *)

let update_current context parse st = 
  let context, contents = parse context (List.enum (List.rev st.current)) in
  context,
  {
    Block.number = (match st.number with
      | Some n -> Some (Numbering.update st.format [n])
      | _ -> None);
    Block.contents; Block.checkbox = st.checkbox
  } :: st.items
    
let interrupt context st parse =
  let context, items = update_current context parse st in
  context, [Block.List (List.rev items, st.ordered)]

(* Saute les espaces au début d'une chaîne *)
let rec skip k indent string = 
  if indent = Some 0 then Some k, Some (String.lchop ~n: k string)
  else if k = String.length string then Some k, Some ""
  else if string.[k] = ' ' then
    skip (k+1) (Option.map pred indent) string
  else if indent = None then
    Some k, Some (String.lchop ~n: k string)
  else
    indent, None

let parse_line st { Org_automaton.line; Org_automaton.parse; Org_automaton.context } =
  if line = "" then 
    if st.last_line_empty then 
      let context, block = interrupt context st parse in
      context, Org_automaton.Done (block, false)
    else 
      context, Org_automaton.Partial { st with last_line_empty = true;
				       current = "" :: st.current }
  else 
    let st = { st with last_line_empty = false } in
    match parse_first_line line with
    | None ->
      (match skip 0 st.indent line with
      | indent, Some skipped ->
        context, Org_automaton.Partial
          { st with indent; current = skipped :: st.current }
      | _, None ->
        let context, block = interrupt context st parse in
        context, Org_automaton.Done (block, false))
    | Some (_, checkbox, format, contents) ->
      let format' = Option.default st.format format in
      let number = Option.default 0 st.number in
      let context, items = update_current context parse st in
      let st' = { st with
        current = [BatSubstring.to_string contents];
        format = format'; checkbox;
        number = compute_number st.ordered format (number+1);
	indent = None; items
      }
      in context, Org_automaton.Next st'
      
let priority = 10
