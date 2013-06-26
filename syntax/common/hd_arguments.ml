open Batteries
type value = 
| Value of string
| Call of string * (string * value) list


and t = {
  arguments: (string * string list) list;
  vars: (string * value option) list;
  collection: [ `Value | `Output ];
  typ: [ `Table | `List | `Scalar | `FileLink] option;
  format: [ `Raw | `Org | `Html | `Latex | `Code | `PP | `Drawer ] option;
  output: [ `Silent | `Replace | `Append | `Prepend ];
  file: string option;
  filedescr: string option;
  dir: string option;
  exports: [ `Code | `Results ] list;
  tangle: [ `Yes | `No | `File of string ];
  mkdirp: bool;
  comments: [ `No | `Link | `Yes | `Org | `Both | `Noweb ];
  padline: bool;
  expand: bool;
  session: string option;
  noweb: [ `No | `Yes | `Tangle | `No_export | `Strip_export | `Eval ];
  noweb_ref: string option;
  noweb_sep: string;
  cache: bool;
  sep: string option;
  hlines: bool;
  colnames: bool option;
  rownames: bool;
  shebang: string option;
  eval: [ `No | `Query | `Yes];
  wrap: string;
}

let update_vars x f = { x with vars = f x.vars }
let update_collection x f = { x with collection = f x.collection }
let update_typ x f = { x with typ = f x.typ }
let update_format x f = { x with format = f x.format }
let update_output x f = { x with output = f x.output }
let update_file x f = { x with file = f x.file }
let update_filedescr x f = { x with filedescr = f x.filedescr }
let update_dir x f = { x with dir = f x.dir }
let update_exports x f = { x with exports = f x.exports }
let update_tangle x f = { x with tangle = f x.tangle }
let update_mkdirp x f = { x with mkdirp = f x.mkdirp }
let update_comments x f = { x with comments = f x.comments }
let update_padline x f = { x with padline = f x.padline }
let update_expand x f = { x with expand = f x.expand }
let update_session x f = { x with session = f x.session }
let update_noweb x f = { x with noweb = f x.noweb }
let update_noweb_ref x f = { x with noweb_ref = f x.noweb_ref }
let update_noweb_sep x f = { x with noweb_sep = f x.noweb_sep }
let update_cache x f = { x with cache = f x.cache }
let update_sep x f = { x with sep = f x.sep }
let update_hlines x f = { x with hlines = f x.hlines }
let update_colnames x f = { x with colnames = f x.colnames }
let update_rownames x f = { x with rownames = f x.rownames }
let update_shebang x f = { x with shebang = f x.shebang }
let update_eval x f = { x with eval = f x.eval }
let update_wrap x f = { x with wrap = f x.wrap }


let default = {
  vars = [];
  collection = `Value;
  typ = None;
  format = None;
  output = `Replace;
  file = None;
  dir = None;
  exports = [`Code];
  tangle = `No;
  mkdirp = false;
  comments = `No;
  padline = true;
  expand = true;
  filedescr = None;
  session = None;
  noweb = `No;
  noweb_ref = None;
  noweb_sep = "\n";
  cache = false;
  sep = None;
  hlines = false;
  colnames = None;
  rownames = false;
  shebang = None;
  eval = `Yes;
  wrap = "RESULTS";
  arguments = [];
}
  
let delimiters_table = 
  [ '(', (')', false);
    ',', (',', false);
    '"', ('"', true) ]
module D = Delimiters.Make (struct let table = delimiters_table end)

let take_while f s = 
  let open BatSubstring in
  let rec aux k = 
    if length s <= k then s, empty ()
    else if f (get s k) then aux (k+1)
    else split_at k s
  in aux 0

open BatSubstring
let rec parse_var sub =
  if length sub = 0 then failwith "empty var"
  else if get sub 0 = '"' then
    match D.enclosing_delimiter ~valid: false sub '"' with
    | None -> failwith "Unterminated quote"
    | Some (quote, rest) -> Value quote, trim rest
  else
    let value, rest = take_while (fun c -> Char.is_digit c || Char.is_latin1 c) sub in
    let rest = trim rest in
    if length rest > 0 && get rest 0 = '(' then
      match D.enclosing_delimiter ~valid: false rest '(' with
      | None -> failwith "Unterminated parenthesis"
      | Some (contents, rest) ->
        let args = D.split ~valid: false (all contents) ',' in
        Call (to_string value, List.map (all %> parse_binding) args), trim rest
    else
      Value (to_string value), rest
and parse_binding sub = try
  Scanf.sscanf (to_string sub) "%[^=] = %[^\n]" 
    (fun a b -> (String.trim a, fst (parse_var (all b))))
  with _ -> failwith ("Invalid binding ("^to_string sub^")")
and parse_binding_opt sub = try
  Scanf.sscanf (to_string sub) "%[^=] = %[^\n]" 
    (fun a b -> (String.trim a, Some (fst (parse_var (all b)))))
  with _ -> to_string sub, None

let table' =
    [ '(', (')', false);
      '"', ('"', true);
      ' ', (' ', false); ]
module D' = Delimiters.Make (struct let table = table' end)
let get_args s = 
  let l = D'.split ~valid: false (all s) ' ' in
  let rec aux list name acc = function
    | [] -> (name, List.rev acc) :: list
    | t :: q when  t <> "" && t.[0] = ':' ->
      let list = if name <> "" then (name, List.rev acc) :: list else list in
      aux list (String.lchop t) [] q
    | t :: q ->
      aux list name (t :: acc) q
  in aux [] "" [] l
  
  
let whd l x = if l = [] then x else List.hd l
let whd_map f l x = if l = [] then x else try f (List.hd l) with _ -> x
let whd_ table l x = 
  if l = [] then x else try List.assoc (List.hd l) table with Not_found -> 
    Log.warning "Expected value among: %s. Got: %s." 
      (String.concat ", " (List.map fst table)) (List.hd l); x

let whd_opt l x = if l = [] then x else Some (List.hd l)
let whd_bool = whd_map (function "yes" -> true | "no" -> false)
let ( @|@ ) g f = fun l x -> g x (f l)
let options = [
  "var", update_vars @|@ (fun l y -> List.map (all %> parse_binding_opt) l @ y);
  "results", (fun x f -> f x) @|@ (fun l x ->
    let handle_word x = function
      | "value" -> { x with collection = `Value }
      | "output" -> { x with collection = `Output }
      | "table" | "vector" -> { x with typ = Some `Table }
      | "list" -> { x with typ = Some `List }
      | "scalar" | "verbatim" -> { x with typ = Some `Scalar }
      | "file" -> { x with typ = Some `FileLink }
      | "raw" -> { x with format = Some `Raw }
      | "html" -> { x with format = Some `Html }
      | "latex" -> { x with format = Some `Latex }
      | "pp" -> { x with format = Some `PP }
      | "code" -> { x with format = Some `Code }
      | "org" -> { x with format = Some `Org }
      | "drawer" -> { x with format = Some `Drawer }
      | "silent" -> { x with output = `Silent }
      | "prepend" -> { x with output = `Prepend }
      | "append" -> { x with output = `Append }
      | "replace" -> { x with output = `Replace }
      | _ -> x
    in List.fold_left handle_word x l);
  "file", update_file @|@ whd_opt;
  "file-desc", update_filedescr @|@ whd_opt;
  "dir", update_dir @|@ whd_opt;
  "exports", update_exports 
    @|@ whd_ ["code", [`Code]; "none", []; 
              "results", [`Results]; "both", [`Code; `Results]] ;
  "tangle", update_tangle @|@ whd_map (function "no" -> `No | "yes" -> `Yes | s -> `File s);
  "mkdirp", update_mkdirp @|@ whd_bool;
  "comments", update_comments @|@ whd_
  ["no", `No; "link", `Link; "yes", `Yes; "org", `Org; "both", `Both;
   "noweb", `Noweb];
  "padline", update_padline @|@ whd_bool;
  "no_expand", update_expand @|@ (fun _ _ -> false);
  "session", update_session @|@ (fun l _ -> if l = [] then Some "" else Some (List.hd l));
  "noweb", update_noweb @|@ whd_
      ["no", `No; "yes", `Yes; "tangle", `Tangle; "no-export", `No_export;
       "strip-export", `Strip_export; "eval", `Eval];
  "noweb-ref", update_noweb_ref @|@ whd_opt;
  "noweb-sep", update_noweb_sep @|@ whd;
  "cache", update_cache @|@ whd_bool;
  "sep", update_sep @|@ whd_opt;
  "hlines", update_hlines @|@ whd_bool;
  "colnames", update_colnames @|@ whd_map (function "yes" -> Some true | "no" -> Some false);
  "rownames", update_rownames @|@ whd_bool;
  "shebang", update_shebang @|@ whd_opt;
  "eval", update_eval @|@ whd_
      ["never", `No; "no", `No; "query", `Query; "never-export", `No; "no-export", `No;
       "query-export", `Query];
  "wrap", update_wrap @|@ whd
]
  
  
  
  
      
let parse string = 
  let args = get_args string in
  List.fold_left (fun x (name, args) ->
    try
      List.assoc name options args x
    with Not_found -> Log.warning "Ignored unkonwn option %s" name; x
    | _ -> x) { default with arguments = args } args

let to_string x = 
  String.concat " "
    (List.map (fun (a, b) ->
      Printf.sprintf ":%s %s" a 
        (String.concat " " b))
       x.arguments)
