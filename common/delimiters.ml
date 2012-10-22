open Batteries
open Prelude

(* To use s.[k] with a substring *)
module String = BatSubstring
open BatSubstring
module type Table = sig
  val table : (char * (char * bool)) list
end

module Make (T : Table) = struct
  (* Some alises function *)
  let is_delimiter c = List.mem_assoc c T.table
  let closing c = try fst (List.assoc c T.table) with Not_found -> c
  let quote c = try snd (List.assoc c T.table) with Not_found -> false

  
  let valid_delimiter s k = 
    let isspace c = c = ' ' || c = '\t' in
    (* tell if s.[k] is in the middle of spaces *)
    let surrounded_by_space () = match s.[k-1], s.[k+1] with
      | c, c' when isspace c && isspace c' -> false
      | _ -> true
    in
    (* a delimiter is valid if: *)
    (* it is not escaped *)
    not (is_escaped s k)
    && (* not in the middle of spaces *)
    (k = 0 || k = String.size s - 1 || surrounded_by_space ())

  (* Given a string [s] and a character [c], returns the index of the closing
     delimiters associated to [c] (or None if not found). The optional parameter
     k tells where the search should start *)
  let rec closing_delimiter  ?(k=0) s c =
    let q = quote c in
    let c = closing c in
    let rec aux k = 
      if k = String.size s then None
      else if s.[k] = c && valid_delimiter s k then Some k
      else if not q && is_delimiter s.[k] && closing s.[k] <> s.[k]  && valid_delimiter s k then
        match closing_delimiter ~k:(k+1) s s.[k] with
          | Some k' -> aux (1+k')
          | None ->    aux (k+1)
      else aux (k+1)
    in aux k

  let enclosing_delimiter string delimiter = 
    if String.length string = 0 || string.[0] <> delimiter then None
    else
      match closing_delimiter string ~k: 1 delimiter with
        | None -> None
        | Some k -> 
          let s, a, b = base string in
          Some (Batteries.String.sub s (a+1) (k-1), 
                triml (k+1) string)

  let split string delimiter = 
    let open Prelude in
        let sub s l = to_string (trimr (size s - l) s) in
        let rec aux acc string = 
          match closing_delimiter string delimiter with
            | None -> List.rev (if is_empty string then acc 
              else to_string string :: acc)
            | Some k ->
              aux (sub string k :: acc) (triml (k+1) string)
        in aux [] string
end
