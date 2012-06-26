open Prelude
open Batteries


type t = 
  | Data of string
  | Block of (string * string) list * bool * string * t list

let escape_entities s =
  Str.global_replace (Str.regexp "&") "&amp;"
    (Str.global_replace (Str.regexp "<") "&lt;"
       (Str.global_replace (Str.regexp ">") "&gt;" s))

let block ?(attr = []) ?(indent = true) name children = 
  Block (attr, indent, name, children)
let data s = Data s
let output out = 
  let print_attr l = 
    String.concat " " 
      (List.map (fun (key, value) -> Printf.sprintf "%s=%S" key value) l)
  in
  let print_mark name attr children =
    Printf.fprintf out "<%s%s%s%s>" name (if attr = [] then "" else " ") 
      (print_attr attr)
      (if children = [] then "/" else "")
  in
  let rec aux = function
    | Data s ->
        IO.nwrite out s
    | Block (attr, _, name, []) ->
        print_mark name attr []
    | Block (attr, shallindent, name, children) ->
        print_mark name attr children;
        List.iter aux children;
        Printf.fprintf out "</%s>" name
  in aux
