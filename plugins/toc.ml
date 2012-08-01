(* Toc-based operations *)
open Batteries
open Prelude
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
  let number_format_appendix = Config.add config "appendix-format" string "The format used to number the headings in the appendix" "A.1.1.1.1"
  let number_heading_format = Config.add config "number-heading-format" string 
    "The format in which insert the the number in the heading (beginning)" 
    ~vars: [make_var "number" "The number of the heading"] "$number "
  let number_toc = Config.add config "number-toc" boolean "True if there should be numbers in the table of contents" false
end
open E
let _ = Plugin.General.add (module E : Plugin with type interface = unit)
type item = 
    {name: Inline.t list;
     anchor : string;
     appendix: bool;
     number: string;
     children: t}
and t = item list

let format format numbers = 
  Numbering.update ~trunc: true format numbers
let shall_be_numbered = Document.has_tag "nonumber" |- not
let contains_appendix = 
  List.exists (function
    | Custom ("appendix", _, _) -> true
    | _ -> false)

let gather { get } doc = 
  let appendix = ref false in
  let compute_number numbers is_appendix = 
    format (get (if is_appendix then number_format_appendix 
                                else number_format))
      numbers
  in
  let o = object(self)
    inherit [t, int list ref] Document.bottomUpWithArg
    method bot = []
    method combine = List.concat
    method heading numbers t = 
      if shall_be_numbered t then
        let () = numbers := List.hd !numbers + 1 :: List.tl !numbers in
        let number = compute_number (List.rev !numbers) !appendix 
        and app = !appendix in
        let () = numbers := 1 :: !numbers in
        let item = {
          name = t.Document.name;
          appendix = app; number; 
          anchor = t.Document.anchor;
          children = List.map (self#heading numbers) t.Document.children 
                       |> List.concat
        }
        in
        (if List.length !numbers >= 2 then numbers := List.tl !numbers;
         appendix := !appendix || contains_appendix t.Document.content;
         if contains_appendix t.Document.content then
           numbers := [0];
         [item])
      else
        []
  end
  in
  let r = ref [0] in
  List.map (o#heading r) doc.Document.headings |> List.concat

let link s =
  let map_char = function 
    | ('a'..'z' | 'A'..'Z' | '0' .. '9') as c -> String.of_char c
    | ' ' | '_' -> "_" | c -> Printf.sprintf "-%x-" (int_of_char c)
  in String.to_list s |> List.map map_char  |> String.concat ""

let generate { get } toc =
  let rec aux numbers l = 
  Block.List (List.mapi (fun k {name; anchor; children; number} ->
    let number = if get number_toc then Some number else None in
    let name = Link {url = Search anchor; label = name} in
    { contents = [Paragraph [name]; aux (numbers @ [1+k]) children];
      checkbox = None;
      Block.number }) l, get number_toc)
  in aux [] toc

let transform ({ get } as conf) doc =
  let global_toc = gather conf doc in
  let o = object(self)
    inherit [int list * t ref] Document.mapper as super
    method heading (numbers, toc) t = 
      if shall_be_numbered t then
        let entry = List.hd !toc in
        let echildren = ref entry.children in
        let () = (toc := List.tl !toc) in
        let children = List.mapi (fun k i ->
          self#heading (numbers @ [1+k], echildren) i)
          t.Document.children
        in
        let assoc l s = try List.assoc s l with _ -> "" in
        let prefix = substitute (assoc ["number", entry.number]) 
          (get number_heading_format) in
        let name = Plain prefix :: t.Document.name in
        { t with 
          Document.name; Document.children;
          Document.content = 
            self#blocks (numbers, toc) t.Document.content
        }
      else
        t
    method document (_, toc) doc = 
      if get number_heading then
        {doc with Document.headings = 
            List.mapi (fun k i -> self#heading ([1+k], toc) i) doc.Document.headings;
          beginning = self#blocks ([], toc) doc.Document.beginning
        }
      else
        doc
    method block l = function
      | Custom ("tableofcontents", opts, contents) ->
          Custom ("tableofcontents", opts,
                  contents @ [self#block l (generate conf global_toc)])
      | x -> super#block l x
  end in o#document ([], ref global_toc) doc

              
let rec mem_item name toc = 
  name = Inline.asciis toc.name
  || mem name toc.children
and mem name = List.exists (mem_item name)


