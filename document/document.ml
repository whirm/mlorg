(** A high level structure describing a file *)

(** In this file we describe a higher level structure {!Block.t}, which consist
    in a tree formed by headings. *)
open Batteries
open Prelude
(** {1 Type definitions} *) 
type meta = { 
  timestamps : Timestamp.t list;
  (** The plain timestamps appearing in the heading *)
  ranges     : Timestamp.range list;
  (** The timestamp ranges  appearing in the heading *)
  scheduled  : Timestamp.t list;
  (** The SCHEDULED item appearing in the heading *)
  deadlines  : Timestamp.t list;
  (** The deadlines appearing in the heading *)
  footnotes  : (string * Inline.t list) list;
  (** The footnotes defined in that heading *)
  properties : (string * string) list;
  (** The properties of the heading *)
  clocks: Timestamp.range list;
  (** The clocked amount of time *)
  current_clock : Timestamp.t option;
  (** The optional time when it was clocked *)
}
(** The metadata of a heading in a document. *)

type heading = {
  name       : Inline.t list;
  level      : int;
  content    : Block.t list;
  mutable father : heading option;
  children   : heading list;
  tags       : string list;
  marker     : string option;
  priority  : char option; (** The optional priority *)
  meta       : meta;
  anchor     : string;
}
(** A heading in a document *)


type t = { 
  filename : string;
  (** The filename the document was parsed from *)
  beginning : Block.t list;
  (** The contents at the beginning *)
  directives : (string * string) list;
  (** The directives present in the file *)
  headings : heading list;
  (** The top-level heading *)
  beg_meta : meta;
  (** The timestamp present in the beginning *)
  exts : string list;
  (** The extensions used by the documents *)
  opts : (string * string) list;
  (** The options set by the file *)
  title: string;
  (** The document's title *)
  author: string;
  (** The document's author *)

}
(** 
    A document is:
    - some content before the first heading
    - a list of top-level headings
    - a list of directive
    - the footnotes inside the beginning of the file.
    - the extensions to load to process this document
    - the options for extensions on this document. Note that this cover as well
      options for exporters that do not appear in [exts]
*)
    
(** {1 Mapping and folding} *)
let map f v l = List.map (f v) l
class ['a] mapper = object(self)
  inherit ['a] Block.mapper
  method document (v: 'a) d = 
    { d with headings = map self#heading v d.headings;
      beginning = self#blocks v d.beginning
    }
  method heading v h = 
    { h with name = self#inlines v h.name;
      children = map self#heading v h.children;
      content = self#blocks v h.content
    } 
end
let fold = List.fold_left
class ['a] folder = object(self)
  inherit ['a] Block.folder
  method document (v: 'a) d = 
    fold self#heading (self#blocks v d.beginning) d.headings
  method heading v h = 
    self#inlines
      (fold self#heading 
         (self#blocks v h.content) 
         h.children) 
      h.name
end

class virtual ['a] bottomUp = object(self)
  inherit ['a] Block.bottomUp
  method document d = 
    let a = self#blocks d.beginning in
    let b = List.map self#heading d.headings in
    self#combine (a :: b)
  method heading h = 
    let a = self#inlines h.name in
    let b = self#blocks h.content in
    self#combine (a :: b ::
                    List.map self#heading h.children)
end

class virtual ['a, 'b] bottomUpWithArg = object(self)
  inherit ['a, 'b] Block.bottomUpWithArg
  method document arg d = 
    let a = self#blocks arg d.beginning in
    let b = List.map (self#heading arg) d.headings in
    self#combine (a :: b)
  method heading arg h = 
    let a = self#inlines arg h.name in
    let b = self#blocks arg h.content in
    self#combine (a :: b ::
                    List.map (self#heading arg) h.children)
end
  

(** {1 Importing a tree} *)
let empty_meta = {
  timestamps = []; ranges = []; scheduled = []; deadlines = []; footnotes = []; properties = [];
  clocks = []; current_clock = None;
}

let collect = 
  let orphans = ref [] in
  let collector = object(self)
    inherit [meta] Block.folder as super
    method block meta = function
      | Block.Property_Drawer p -> 
        { meta with properties = p @ meta.properties }
      | Block.Footnote_Definition (name, def) ->
          if meta.footnotes |> List.exists (fst |- (=) name) then
            { meta with footnotes = meta.footnotes |> 
                List.map (fun (name', v) -> 
                  name, if name' = name then def else v) }
          else (orphans := (name, def) :: !orphans; meta)
      | block -> super#block meta block (* no recursion *)
    method inline meta = 
      let open Inline in function
        | Timestamp (Date t) -> { meta with timestamps = t :: meta.timestamps }
        | Timestamp (Scheduled t) -> { meta with scheduled = t :: meta.scheduled }
        | Timestamp (Deadline t) -> { meta with deadlines = t :: meta.deadlines }
        | Timestamp (Range t) -> { meta with ranges = t :: meta.ranges }
        | Timestamp (Clock (Stopped t)) -> { meta with clocks = t :: meta.clocks }
        | Timestamp (Clock (Started t)) -> { meta with current_clock = Some t }
        | Footnote_Reference ({name; definition = Some def}) ->
          { meta with footnotes = (name, def) :: meta.footnotes }
        | Footnote_Reference ({name}) ->
            (try
              { meta with footnotes = (name, List.assoc name !orphans) :: meta.footnotes }
            with Not_found ->
              { meta with footnotes = (name, []) :: meta.footnotes })
        | x -> super#inline meta x
  end
  in
  collector#blocks empty_meta

let gather_keywords doc = 
  let gatherer = object(self) 
    inherit [unit] mapper as super
    method blocks () = 
    let open Block in
        function
          | With_Keywords (l, Paragraph []) :: With_Keywords (l', b) :: q ->
              self#blocks () (With_Keywords (l @ l', b) :: q)
          | With_Keywords (l, Paragraph []) :: other :: q ->
              With_Keywords (l, other) :: self#blocks () q
          | t :: q -> super#block () t :: self#blocks () q
          | [] -> []
  end in
  gatherer#document () doc
(* The following function takes a list of blocks, and returns
- the blocks until next heading
- the heading
- the rest *)

let look_for_heading = 
  let open Block in
      let rec aux acc = function
        | Heading t :: q -> (List.rev acc, Some t, q)
        | t :: q -> aux (t :: acc) q
        | [] -> List.rev acc, None, []
      in aux []

(* [handle_directives doc] analyses the directives of a document to get the list of
extensions to load *)
let handle_directives doc = 
  { doc with exts = (try words (List.assoc "extensions" doc.directives)
    with _ -> []);
    opts = List.filter (fun (s, _) -> String.exists s ".") doc.directives
 }


(* [from_blocks filename blocks] transforms the list of blocks [blocks] into a
   structured document corresponding to filename [filename]. *)
let from_blocks filename blocks = 
  (* [leave_heading] is called when the end of a heading is found, it updates the
     fields that need to be : the metadata, and the children (that are in reverse
     order). It takes an extra parameter which is the content of the heading. *)
  let leave_heading heading c =
    let heading = 
      { heading with children = List.rev heading.children;
        content = c; 
        meta = collect c }
    in 
    List.iter (fun c -> c.father <- Some heading) heading.children;
    heading
  in
  let directives = 
    let o = object
      inherit [(string * string) list] Block.folder as super
      method block l = function
        | Block.Directive (a, b) -> (a, b) :: l
        | x -> super#block l x
    end
    in o#blocks [] blocks 
  in
(* The recursive function processing block by block. [has_contents] tells
  whether the current block has already some content.
  The algorithm is simple:
  - Look for the next heading, if there is none, we are done.
  - If there is a heading whose level is less than ours' then return as well,
    because our heading stops here
  - Otherwise, parse this heading in a recursive call, and continue *)
  let rec aux has_contents heading blocks = 
    let up_contents c = if has_contents then heading.content
      else c
    in
    match look_for_heading blocks with
      | all, None, _ -> 
        leave_heading heading (up_contents all), []
          
      | start, Some ({Block.level = k} as h), rest ->
        if k > heading.level then
          let child, rest = 
            aux false { name = h.Block.title; father = None;
                        anchor = Inline.asciis h.Block.title;
                        level = k; content = []; children = []; 
                        tags = h.Block.tags; marker = h.Block.marker;
                        priority = h.Block.priority; meta = empty_meta } 
              rest
          in aux true { heading with 
            children = child :: heading.children;
            content = up_contents start
          } rest
        else
          leave_heading heading (up_contents start), 
          (Block.Heading h :: rest)
  in
  (* In the end, don't forget to parse the directives *)
  let main, _ = 
    aux false { name = []; level = 0; content = []; father = None;
                anchor = ""; priority = None;
                children = []; tags = []; marker = None;
                meta = empty_meta } blocks
  in
  gather_keywords 
    (handle_directives
       { beginning = main.content; directives;
         exts = []; opts = [];
         headings = main.children;
         beg_meta = main.meta;
         title = (try List.assoc "TITLE" directives with _ -> "");
         author = (try List.assoc "AUTHOR" directives with _ -> "");
         filename;
       })
(** {1 Parsing from files} *)
let from_chan filename channel = 
    BatIO.lines_of channel |> 
    Org_parser.parse |> snd |>
    from_blocks filename

let from_file filename = 
    BatFile.with_file_in filename (from_chan filename)


let rec descendants heading = 
  heading :: List.concat (List.map descendants heading.children)

let name h = Inline.asciis h.name
let father x = x.father
let prop_val name h =
  try Some (List.assoc name h.meta.properties)
  with Not_found -> None
let prop_val_ name h =
  List.assoc name h.meta.properties
  

let dump = 
  let rec aux i = 
    List.iter (fun heading ->
      for j = 1 to i do
        print_char ' '
      done;
      Printf.printf "%s (%s)\n" (Inline.asciis heading.name)
        (match heading.father with
          | Some f -> Inline.asciis f.name
          | None -> "");
      aux (i + 4) heading.children)
  in aux 0

let blocks_by_keywords pred doc =
  let folder = object(self)
    inherit [Block.t list] folder as super
    method block acc = function
      | Block.With_Keywords (l, t) when pred l ->
        self#block (t :: acc) t
      | x -> super#block acc x
  end
  in folder#document [] doc


let find_block_by_name name doc = 
  match blocks_by_keywords 
    (fun l -> try List.assoc "NAME" l = name with _ -> false) doc with
    | x :: _ -> Some x
    | _ -> None

let current_clocked_item doc =
  let o = object(self)
    inherit [heading option] folder as super
    method heading v h = match h.meta.current_clock with
      | Some c -> Some h
      | None -> super#heading v h
  end
  in o#document None doc

let clocking_time h = 
  List.fold_left ( + )
    (Option.map_default Timestamp.from_now 0  h.meta.current_clock)
    (List.map Timestamp.duration h.meta.clocks)
let current_clocking_time doc = Option.map clocking_time (current_clocked_item doc)

let rec has_tag t h = 
  List.mem t h.tags || Option.map_default (has_tag t) false h.father

let rec has_tag t h = 
  List.mem t h.tags || Option.map_default (has_tag t) false h.father

let rec has_tag t h = 
  List.mem t h.tags || Option.map_default (has_tag t) false h.father

let footnotes doc = 
  let rec handle_heading h = 
    h.meta.footnotes @ List.concat (List.map handle_heading h.children)
  in
  doc.beg_meta.footnotes @ List.concat (List.map handle_heading doc.headings)
  
