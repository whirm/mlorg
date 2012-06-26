open Batteries
open Prelude

(* To use s.[k] *)
module String = BatSubstring
open BatSubstring
module type Table = sig
  val table : (char * (char * bool)) list
end

module Make (T : sig val table : (char * (char * bool)) list end) = struct
  let is_delimiter c = List.mem_assoc c T.table
  let is_symbol c =
    is_delimiter c || List.mem c (List.map (snd |- fst) T.table)
  let delimiter_data c = 
    let rec aux = function
      | [] -> c, true
      | (op, (cl, d)) :: _ when c = cl || c = op -> (cl, d)
      | _ :: q -> aux q
    in aux T.table
  
  let closing = fst -| delimiter_data 
  let shall_be_in_word = snd -| delimiter_data
  let valid_delimiter s k = 
    let isspace c = c = ' ' || c = '\t' in
    (* for a delimiter to be valid,
       we don't want it to be surrounded by two spaces OR two delimiters
       but a delimiter and a space is ok. *)
    let striking () = match s.[k-1], s.[k+1] with
      | c, c' when isspace c && isspace c' -> false
      | c, c' when is_symbol c && is_symbol c' -> false
      | _ -> true
    in
    (not (shall_be_in_word s.[k]) 
    || (k = 0 || k = String.size s - 1
          || striking ()))
      && not (is_escaped s k)
  let rec closing_delimiter  ?(k=0) s c = 
    let c = closing c in
    let rec aux k = 
      if k = String.size s then None
      else if s.[k] = c && valid_delimiter s k then Some k
      else if is_delimiter s.[k] && closing s.[k] <> s.[k]  && valid_delimiter s k then
        match closing_delimiter ~k:(k+1) s s.[k] with
          | Some k' -> aux (1+k')
          | None ->    aux (k+1)
      else aux (k+1)
    in aux k

  let first_delimiter s = 
    let rec aux k = 
      if k = String.size s then None
      else if is_delimiter s.[k] && valid_delimiter s k then
        Some k
      else aux (k+1)
    in aux 0

  let remove_delimiter s k = 
    let before, after = String.split_at (k+1) s in
    String.trimr 1 before, after

  let enclosing_delimiter string delimiter = 
    if string.[0] <> delimiter then None
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
