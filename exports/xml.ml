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
  let spaces k = 
    for i = 0 to k-1 do
      IO.write out ' '
    done
  in
  let print_attr l = 
    String.concat " " 
      (List.map (fun (key, value) -> Printf.sprintf "%s=%S" key value) l)
  in
  let rec aux indent = function
    | Data s ->
      List.iter (fun s -> IO.nwrite out s) (lines s)
    | Block (attr, _, name, []) ->
      spaces indent;
      IO.printf out "<%s %s/>\n" name (print_attr attr)
    | Block (attr, shallindent, name, children) ->
      spaces indent;
      IO.printf out "<%s %s>" name (print_attr attr);
      List.iter (aux (if shallindent then indent + 2 else 0)) children;
      IO.nwrite out "\n";
      spaces indent;
      IO.printf out "</%s>\n" name
  in aux 0
