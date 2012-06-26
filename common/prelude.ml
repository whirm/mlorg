open Batteries

let words s = String.nsplit s " "
let lines s = String.nsplit s "\n"

let filteri f s =
  let r = ref (-1) in
  String.filter (fun _ -> incr r; f !r) s
let rec is_escaped s k = 
  let module String = BatSubstring in
  k < String.size s && k > 0 && s.[k-1] = '\\' 
  && not (is_escaped s (k-1))
let is_escaping s k = is_escaped s (k+1)
let unescape ?(l = []) s = 
  filteri (fun k -> not (is_escaping s k) ||  not (List.mem (BatSubstring.get s (k+1)) l))
    (BatSubstring.to_string s)


let change_ext ext file =
  if file = "-" then file
  else Filename.chop_extension file ^ "." ^ ext


let rec concat_fmt f sep ppf l = match l with
  | [] -> ()
  | [x] -> f ppf x
  | t :: q -> Format.fprintf ppf "%a%s%a" f t sep 
      (concat_fmt f sep) q
let escape chars s = 
  let regexp =
    Str.regexp
      (String.concat "\\|" (List.map Str.quote chars))
  in
  Str.global_replace regexp "\\\\\\0" s

let substitute f s = 
  let b = Buffer.create (String.length s) in
  Buffer.add_substitute b f s;
  Buffer.contents b
