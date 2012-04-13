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
}
(** The metadata of a heading in a document. *)

type heading = {
  name      : Inline.t list;
  level     : int;
  content   : Block.t list;
  children  : heading list;
  tags      : string list;
  marker    : string option;
  meta      : meta;
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
  ext_opts : (string * string) list
  (** The options of the extensions *)
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

      Mappers may carry top-down information:
#+begin_src ocaml

  let map f v l = List.map (f v) l
  class ['a] mapper = object(self)
    inherit ['a] Block.mapper
    method document (v: 'a) d = 
      { d with headings = map self#heading v d.headings;
        beginning = map self#block v d.beginning
      }
    method heading v h = 
      { h with name = map self#inline v h.name;
        children = map self#heading v h.children;
        content = map self#block v h.content
      } 
  end
  let fold = List.fold_left
  class ['a] folder = object(self)
    inherit ['a] Block.folder
    method document (v: 'a) d = 
      fold self#heading (fold self#block v d.beginning) d.headings
    method heading v h = 
      fold self#inline 
        (fold self#heading 
           (fold self#block v h.content) 
           h.children) 
        h.name
  end
    
    
  
#+end_src

(** {1 Importing a tree} *)
This section converts a bunch of blocks into a tree.

The first function collects metadata contained in a bunch of blocks.

#+begin_src ocaml
  let collect = 
    let collector = object(self)
      inherit [meta] Block.folder as super
      method inline meta = 
        let open Inline in function
          | Timestamp (Date t) -> { meta with timestamps = t :: meta.timestamps }
          | Timestamp (Scheduled t) -> { meta with scheduled = t :: meta.scheduled }
          | Timestamp (Deadline t) -> { meta with deadlines = t :: meta.deadlines }
          | Timestamp (Range t) -> { meta with ranges = t :: meta.ranges }
          | Footnote ({name = Some name; contents = Some def}) ->
            { meta with footnotes = (name, def) :: meta.footnotes }
          | x -> super#inline meta x
    end
    in
    let default_meta = {
      footnotes = []; scheduled = []; deadlines = [];
      ranges = []; timestamps = [] 
    } in
    collector#blocks default_meta
#+end_src

The following function takes a list of blocks, and returns
- the blocks until next heading
- the heading
- the rest

#+begin_src ocaml
  let look_for_heading = 
    let open Block in
    let rec aux acc = function
      | Special (Heading t) :: q -> (List.rev acc, Some t, q)
      | t :: q -> aux (t :: acc) q
      | [] -> List.rev acc, None, []
    in aux []
#+end_src


=handle_directives doc= analyses the directives of a document to get the list of
extensions to load:
#+begin_src ocaml
  let handle_directives doc = 
    { doc with exts = (try words (List.assoc "extensions" doc.directives)
                      with _ -> []);
      ext_opts = List.filter_map (fun (name, value) ->
        try Scanf.sscanf name "opt_%s" (fun n -> Some (n, value))
        with _ -> None) doc.directives }
                        
  
#+end_src

Empty metadata for headings :
#+begin_src ocaml
  let empty_meta = {
    timestamps = []; ranges = []; scheduled = []; deadlines = []; footnotes = [] 
  }
#+end_src

=from_blocks filename blocks= transforms the list of blocks =blocks= into a structured document
corresponding to filename =filename=.
#+begin_src ocaml
  let from_blocks filename blocks = 
#+end_src
- The directives of the document, in a reference. The function token2block conerts
  a token (that is a special line or a convential block) into a block.  It ignores
  heading and directives (but performs the expected side-effect on =directives=).
  #+begin_src ocaml
      let directives = ref [] in
      let token2block = 
        List.filter_map (function
          | Block.Special (Block.Directive (a, b)) -> 
            directives := (a, b) :: !directives; None
          | Block.Special _ -> None
          | Block.Block t -> Some t)
      in
  #+end_src
- =leave_heading= is called when the end of a heading is found, it updates the
  fields that need to be : the metadata, and the children (that are in reverse
  order). It takes an extra parameter which is the content of the heading.
  #+begin_src ocaml
    let leave_heading heading c =
      { heading with children = List.rev heading.children;
        content = c;
        meta = collect c}
    in
  #+end_src
- The recursive function processing block by block. =has_contents= tells
  whether the current block has already some content.
  The algorithm is simple:
  - Look for the next heading, if there is none, we are done.
  - If there is a heading whose level is less than ours' then return as well,
    because our heading stops here
  - Otherwise, parse this heading in a recursive call, and continue
  #+begin_src ocaml
    let rec aux has_contents heading blocks = 
      let up_contents c = if has_contents then heading.content
        else token2block c
      in
      match look_for_heading blocks with
      | all, None, _ -> 
        leave_heading heading (up_contents all), []
    
      | start, Some ({Block.level = k} as h), rest ->
        if k > heading.level then
          let child, rest = 
            aux false { name = h.Block.title;
                       level = k; content = []; children = []; 
                       tags = h.Block.tags; marker = h.Block.marker;
                       meta = empty_meta } rest
          in aux true { heading with 
            children = child :: heading.children;
            content = up_contents start
          } rest
        else
          leave_heading heading (up_contents start), 
          (Block.Special (Block.Heading h) :: rest)
    in
  #+end_src
- In the end, don't forget to parse the directives
  #+begin_src ocaml
    let main, _ = 
      aux false { name = []; level = 0; content = [];
                  children = []; tags = []; marker = None;
                  meta = empty_meta } blocks
    in
    handle_directives
      { beginning = main.content;
        directives = !directives;
        exts = []; ext_opts = [];
        headings = main.children;
        beg_meta = main.meta;
        filename;
    }
    
  #+end_src

** Parsing from files
The following function parses a document from a channel.
#+begin_src ocaml
let from_chan filename channel = 
    BatIO.lines_of channel |> 
    Parser.parse |> 
    List.of_enum |>
    List.concat |>
    from_blocks filename
#+end_src

This one parses from a file:
#+begin_src ocaml
let from_file filename = 
    BatFile.with_file_in filename (from_chan filename)
#+end_src
