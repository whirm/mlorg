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
    else if s.[0] = '+' || s.[0] = '-' then Some (1, false)
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

let is_start { Automaton.line } = 
  match parse_first_line line with
    | None -> None
    | Some (ordered, checkbox, format, contents) ->
      let format' = Option.default "1" format in
      Some { items = []; last_line_empty = false;
             current = [BatSubstring.to_string contents]; 
             format = format'; checkbox; ordered;
             number = compute_number ordered format 1;
           }
(** is_start basically checks if the line is a valid star for an item *)

let update_current parse st = 
  {
    Block.number = (match st.number with
      | Some n -> Some (Numbering.update st.format [n])
      | _ -> None);
    Block.contents = parse (List.enum (List.rev st.current));
    Block.checkbox = st.checkbox
  } :: st.items

let interrupt st parse =
    [Block.List (List.rev (update_current parse st), st.ordered)]

let parse_line st { Automaton.line; Automaton.parse } =
  if line = "" then 
    if st.last_line_empty then Automaton.Done (interrupt st parse, false)
    else Automaton.Partial { st with last_line_empty = true }
      
  else match parse_first_line line with
    | None ->
      if String.starts_with line "  " then
        Automaton.Partial
          { st with current = String.lchop ~n:2 line :: st.current }
      else
        Automaton.Done (interrupt st parse, false)
    | Some (_, checkbox, format, contents) ->
      let format' = Option.default st.format format in
      let number = Option.default 0 st.number in
      let st' = { st with
        current = [BatSubstring.to_string contents];
        format = format'; checkbox;
        number = compute_number st.ordered format (number+1);
        items = update_current parse st
      }
      in Automaton.Partial st'
      
let priority = 10
