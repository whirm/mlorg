(* Toc-based operations *)
open Batteries
open Inline
open Block
open Plugin
open Config

module E = struct
  type interface = unit
  let data = ()
  let name = "toc"
  let config = Config.create ()
  let number_heading = Config.add config "number-heading" boolean "True if headings should be numbered" true
  let number_format = Config.add config "number-format" string "The format used to number the headings" "1.1.1.1.1"
  let number_toc = Config.add config "number-toc" boolean "True if there should be numbers in the table of contents" false
end
open E
let _ = Plugin.General.add (module E : Plugin with type interface = unit)
type item = 
    {name: Inline.t list;
     anchor : string;
     children: t}
and t = item list

let format format numbers = 
  Numbering.update ~trunc: true format numbers

let gather doc = 
  let o = object(self)
    inherit [t] Document.folder
    method heading list t = 
      let item = {
        name = t.Document.name;
        anchor = t.Document.anchor;
        children = List.rev (List.fold_left self#heading [] t.Document.children)
      }
      in
      item :: list
  end
  in
  List.rev (o#document [] doc)
let link s =
  let map_char = function 
    | ('a'..'z' | 'A'..'Z' | '0' .. '9') as c -> String.of_char c
    | ' ' | '_' -> "_" | c -> Printf.sprintf "-%x-" (int_of_char c)
  in String.to_list s |> List.map map_char  |> String.concat ""

let generate { get } toc =
  let rec aux numbers l = 
  Block.List (List.mapi (fun k {name; anchor; children} ->
    let number = if get number_toc then
        Some (format (get number_format) (numbers @ [k+1]))
      else None
    in
    let name = Link {url = Search anchor; label = name} in
    { contents = [Paragraph [name]; aux (numbers @ [1+k]) children];
      checkbox = None;
      number }) l, get number_toc)
  in aux [] toc

let transform ({ get } as conf) doc =
  let o = object(self)
    inherit [int list] Document.mapper as super
    method heading numbers t = 
      {
        (super#heading numbers t) with Document.name = 
          Plain (format (get number_format) numbers ^ " ") :: t.Document.name;
          Document.children = 
          List.mapi (fun k i -> self#heading (numbers @ [1+k]) i) 
            t.Document.children
      }
    method document _ doc = 
      if get number_heading then
        {doc with Document.headings = 
            List.mapi (fun k i -> self#heading [1+k] i) doc.Document.headings;
          beginning = self#blocks [] doc.Document.beginning
}
      else
        doc
    method block l = function
      | Custom ("tableofcontents", opts, contents) ->
          let toc = gather doc in
          Custom ("tableofcontents", opts,
                  contents @ [self#block l (generate conf toc)])
      | x -> super#block l x
  end in o#document [] doc

              
let rec mem_item name toc = 
  name = Inline.asciis toc.name
  || mem name toc.children
and mem name = List.exists (mem_item name)


