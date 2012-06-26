open Batteries
open Document
type t = Document.heading -> bool
type string_matcher = string -> bool
(** The type of a string matcher: a function matching strings *)

let s s = (=) s
let r r s = Str.string_partial_match r s 0
let rs re s = Printf.printf "Matching %s and %s\n" s re; r (Str.regexp re) s

let ( &&& ) f g = fun x -> f x && g x
let ( ||| ) f g = fun x -> f x || g x

let has_property f = 
  fun h ->
    List.exists (fst |- f) h.meta.properties

let prop_val name f = 
  fun h ->
    List.exists (fun (x, y) -> x = name && f y) h.meta.properties

let has_tag f = 
  fun h ->
    List.exists f h.tags

let rec under f = 
  fun h ->
    f h || (match h.father with
      | Some daddy -> under f daddy
      | None -> false)

let name f =
  fun h -> f (Inline.asciis h.name)

let run_headings = List.filter
let rec run_headings_sub f = function
  | [] -> []
  | t :: q -> 
    let l = run_headings_sub f t.children @ run_headings_sub f q in
    if f t then
      t :: l
    else l

let run f d = run_headings_sub f d.headings
