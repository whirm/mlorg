open Prelude
open Batteries

module Substring = BatSubstring
open Substring

type 'a parser = (string -> 'a list) -> Substring.t -> ('a list * Substring.t) option
let rec run_parsers plain parsers string =
  let substring = Substring.all string in
  let myself = run_parsers plain parsers in
  let push acc start stop = 
    if stop = start then
      acc
    else
      let s = unescape (Substring.substring string start (stop - start)) in
      plain s :: acc
  in
  let rec skip_until ?(k = 0) p s =
    if k >= Substring.length s || p (Substring.get s k) then k
    else skip_until ~k:(k+1) p s
  in
  let rec skip_word s = 
    if Substring.length s > 0 && Substring.get s 0 = '\\' then 
      2+skip_word (Substring.triml 2 s)
    else
      skip_until (fun c -> not (Char.is_whitespace c && Char.is_newline c)) s
        ~k: (skip_until ~k:1 (fun c -> not (Char.is_latin1 c || Char.is_digit c)) s) 
  in
  let rec aux start acc substring = 
    let (_, current, _) = Substring.base substring in
    if Substring.is_empty substring then 
      List.rev (push acc start current)
    else
      let rec try_parsers = function
        | [] -> 
            let lg = skip_word substring in
            aux start acc
              (Substring.triml lg substring)
        | t :: q -> try 
                      match t myself substring with
                        | None -> try_parsers q
                        | Some (r, cont) ->
                          let _, new_off, _ = Substring.base cont in
                            aux new_off (r @ push acc start current) cont
          with _ -> try_parsers q
      in try_parsers parsers
  in aux 0 [] substring

(** {2 Interesting routines} *)
open Option
let inside table delim rest = 
  let module D = Delimiters.Make (struct let table = table end) in
  if Substring.is_empty rest then None, rest
  else match D.enclosing_delimiter rest delim with
    | Some (c, d) -> Some c, d
    | None -> None, rest

let inside_force table delim rest = 
  match inside table delim rest with
    | Some c, d -> c, d
    | _ -> failwith ""

let see s rest = 
  let (before, after) = Substring.split_at (Batteries.String.length s) rest in
  if Substring.to_string before = s then after
  else raise (Failure "")
let skip ?(n = 1) rest = triml n rest
let until pred rest = 
  splitl (pred %> not) rest |> fun (x, y) -> Substring.to_string x, y
let one_of l c = List.mem c l
let ( ||| ) f g = fun x -> f x || g x
let until_space f = until (Char.is_whitespace ||| f)
