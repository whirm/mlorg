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
  let rewrite_footnotes = Config.add config "rewrite-footnotes" boolean "True if footnotes should be handled" true
  let number_heading = Config.add config "number-heading" boolean "True if headings should be numbered" true
  let heading_format = Config.add config "heading-format" string "The format used to number the headings" "1.1.1.1.1"
  let footnote_format = Config.add config "footnote-format" string "The format used to number the footnotes" "1"
  let appendix_format = Config.add config "appendix-format" string "The format used to number the headings in the appendix" "A.1.1.1.1"
  let footnotes_level = Config.add config "footnotes-level" int "Displays the footnotes at the end of sections of given level"
    2
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

let gather config doc = 
  let appendix = ref false in
  let compute_number numbers is_appendix = 
    format (Config.get config (if is_appendix then appendix_format
                                              else heading_format))
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

let generate config toc =
  let rec aux numbers l = 
  Block.List (List.mapi (fun k {name; anchor; children; number} ->
    let number = if Config.get config number_toc then Some number else None in
    let name = Link {url = Search anchor; label = name} in
    { contents = [Paragraph [name]; aux (numbers @ [1+k]) children];
      checkbox = None;
      Block.number }) l, Config.get config number_toc)
  in aux [] toc

let transform config doc =
  let global_toc = gather config doc in
  let footnotes = Document.footnotes doc 
        |> List.mapi (fun i (a, b) -> (a, (i, b))) in
  let footnotes_seen = ref [] in
  let format_footnote k = 
    format (Config.get config footnote_format) [1+k]
  in
  let o = object(self)
    inherit [int list * t ref] Document.mapper as super
    method make_footnote_label n = "_footnote_"^n
    method flush_footnotes () = 
      if !footnotes_seen <> [] then
        let b = Horizontal_Rule :: 
          List.map (fun (name, contents) ->
            let number, _ = List.assoc name footnotes in
            let s = format_footnote number in
            Paragraph (Target (self#make_footnote_label name) ::
                         Superscript [Plain s] ::
                         Plain " " ::
                         contents)) !footnotes_seen
        in (footnotes_seen := []; b)
      else []
    method inline (numbers, toc) = function
      | Footnote_Reference {Inline.name; definition} as x ->
        (try
          let (number, _) = List.assoc name footnotes in
          Inline.(
            Superscript 
              [Link {
                label = [Plain (format_footnote number)];
                url= Search (self#make_footnote_label name)
              }])
        with Not_found -> 
          Log.warning "Reference to unknown footnote `%s' (It is defined in the same section?)" name;
          x)
      | x -> super#inline (numbers, toc) x
    method block (numbers, toc) = function
      | Footnote_Definition _ -> Paragraph []
      | x -> super#block (numbers, toc) x
    method heading (numbers, toc) t = 
      let () = footnotes_seen := !footnotes_seen @ 
        Document.(t.meta.footnotes) in
      let ft_contents = 
        if Config.get config rewrite_footnotes 
          && t.Document.level <= Config.get config footnotes_level then 
          self#flush_footnotes ()
        else []
      in
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
          (Config.get config number_heading_format) in
        let name = Plain prefix :: t.Document.name in
        { t with 
          Document.name; Document.children;
          Document.content = 
            self#blocks (numbers, toc) t.Document.content @ ft_contents
        }
      else
        { t with Document.content = t.Document.content @ ft_contents }
    method document (_, toc) doc = 
      let open Document in
      let () = footnotes_seen := doc.beg_meta.footnotes in
      let footnotes = 
        if Config.get config rewrite_footnotes then self#flush_footnotes () 
        else [] 
      in
      if Config.get config number_heading then
        {doc with 
          headings = 
            List.mapi (fun k i -> self#heading ([1+k], toc) i) doc.headings;
          beginning = self#blocks ([], toc) doc.beginning @ footnotes}
      else
        { doc with beginning = doc.beginning @ footnotes }
    method block l = function
      | Custom ("tableofcontents", opts, contents) ->
          Custom ("tableofcontents", opts,
                  contents @ [self#block l (generate config global_toc)])
      | x -> super#block l x
  end in o#document ([], ref global_toc) doc

              
let rec mem_item name toc = 
  name = Inline.asciis toc.name
  || mem name toc.children
and mem name = List.exists (mem_item name)


