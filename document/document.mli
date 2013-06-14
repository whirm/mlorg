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
  (** The properties of that heading *)
  clocks: Timestamp.range list;
  (** The clocked amount of time *)
  current_clock : Timestamp.t option;
  (** The optional time when it was clocked *)
}
(** The metadata of a heading in a document. *)

type heading = {
  name      : Inline.t list; (** Its name, as inline formatted content *)
  level     : int; (** Its level (number of star), starts at 1 *)
  content   : Block.t list; (** The content before the first children *)
  mutable father : heading option; (** The optional father of the heading. [None] if it is the root heading. *)
  children  : heading list; (** The children *)
  tags      : string list; (** The tags *)
  marker    : string option; (** The optional marker *)
  priority  : char option; (** The optional priority *)
  meta      : meta; (** The metadata gathered in the heading's content *)
  anchor     : string; (** The anchor  -- to refer to the heading *)
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
  (** The top-level headings *)
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

(** In this section we define document traversal. The idea is to be able to
    completely traverse a document by just specifying the case we are interested
    in, and not having to write the cursion calls each time. *)


class ['a] mapper : object
  inherit ['a] Block.mapper
  method document : 'a -> t -> t
  method heading : 'a -> heading -> heading
end
(** Maps an ast. Takes a value that is passed top-down *)

class ['a] folder : object
  inherit ['a] Block.folder
  method document : 'a -> t -> 'a
  method heading : 'a -> heading -> 'a
end
(** Destruct an ast (fold). Takes a value that is built along the way *)

class virtual ['a] bottomUp : object
  inherit ['a] Block.bottomUp
  method document : t -> 'a
  method heading : heading -> 'a
end
(** BottomUp traversal. Values is generated from the leaf and is combined as we climb up the tree *)

class virtual ['a, 'b] bottomUpWithArg : object
  inherit ['a, 'b] Block.bottomUpWithArg
  method document : 'b -> t -> 'a
  method heading : 'b -> heading -> 'a
end
(** BottomUp traversal with an argument which is passed top-down *)

(** {1 Configuration} *)

val config : Config.t
(** The configuration for the from_* functions *)

(** {1 Parsing documents} *)    

val directives : Block.t list -> (string * string) list
(** Return the directives of a list of blocks *)

val opts : Block.t list -> (string * string) list
(** Return the options set in a list of blocks *)

val from_chan : ?config: Config.instance -> string -> BatIO.input -> t * Config.instance
(** From an input (the second argument is the filename).
    Returns a new instance updated with the parameter defined in the document.
 *)
val from_file  : ?config: Config.instance -> string -> t * Config.instance
(** From a file *)
val from_fun : ?config: Config.instance -> string -> (unit -> string option) -> t * Config.instance
(** From a function *)

(** {1 Handling documents} *)

val descendants : heading -> heading list
(** Returns the descendants of the given heading (including this one) *)

val name : heading -> string
(** Returns the ascii encoding of the name of a heading *)

val father : heading -> heading option
(** Returns the father of a heading *)
 
val prop_val : string -> heading -> string option
(** Returns the value of a property *)

val prop_val_ : string -> heading -> string
(** Returns the value of a property -- may fail*)

val dump : heading list -> unit
(** Dump a list of heading as a tree *)

val blocks_by_keywords : ((string * string) list -> bool) -> t -> Block.t list
(** [blocks_by_property document pred] returns all the block in [document]
    whose keywords satisfy [pred] *)

val find_block_by_name : string -> t -> Block.t option
(** [find_block_by_name document name] finds the first block of [document]
    called [name] *)

val current_clocked_item : t -> heading option
(** Returns the current clocked item of a document, if it exists *)

val clocking_time : heading -> int
(** Returns the total time (in seconds) the heading has been clocked *)

val current_clocking_time : t -> int option
(** Returns the clocking time of the currently clocked in entry *)

val has_tag : string -> heading -> bool
(** [has_tag tag heading] returns true if [heading] has the tag [tag] (or inherits it) *)

val footnotes : t -> (string * Inline.t list) list
(** [footnotes document] returns all the footnotes present in a document by order of first reference. *)

(** {3 Smart constructors} *)
val document: ?filename:string ->
  ?beginning:Block.t list ->
  ?directives:(string * string) list ->
  ?opts:(string * string) list ->
  ?beg_meta:meta ->
  ?exts:string list ->
  ?title:string -> ?author:string -> heading list -> t
(** Create a document *)

val heading: ?timestamps: Timestamp.t list ->
  ?ranges: Timestamp.range list ->
  ?scheduled: Timestamp.t list ->
  ?deadlines: Timestamp.t list ->
  ?properties: (string * string) list ->
  ?footnotes: (string * Inline.t list) list ->
  ?clocks: Timestamp.range list ->
  ?current_clock: Timestamp.t ->
  ?father: heading ->
  ?priority: char ->
  ?anchor: string ->
  ?content: Block.t list ->
  ?marker: string ->
  ?tags: string list ->
  ?children: heading list ->
  level: int -> Inline.t list -> heading
(** Create a heading.

    NB: If you specify any of the optional attribute, this function
    will create the corresponding blocks. For instance if you specify
    [properties], a property drawer will be created and appended to
    [content]. So, make sure [content] does not contain already a
    property drawer. (Otherwise you will have two of them)
*)




