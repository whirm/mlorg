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
  name      : Inline.t list;
  level     : int;
  content   : Block.t list;
  mutable father : heading option; (* mutable to break the recursion during the construction *)
  children  : heading list;
  tags      : string list;
  marker    : string option;
  meta      : meta;
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
class ['a] mapper : object
  inherit ['a] Block.mapper
  method document : 'a -> t -> t
  method heading : 'a -> heading -> heading
end
class ['a] folder : object
  inherit ['a] Block.folder
  method document : 'a -> t -> 'a
  method heading : 'a -> heading -> 'a
end
class virtual ['a] bottomUp : object
  inherit ['a] Block.bottomUp
  method document : t -> 'a
  method heading : heading -> 'a
end

(** {1 Parsing documents} *)    
val from_chan : string -> BatIO.input -> t
(** From an input (the first argument is the filename) *)
val from_file  : string -> t
(** From a file *)

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

val find_block_by_name : t -> string -> Block.t option


val current_clocked_item : t -> heading option
(** Returns the current clocked item of a document, if it exists *)

val clocking_time : heading -> int
(** Returns the total time (in seconds) the heading has been clocked *)

val current_clocking_time : t -> int option
(** Returns the clocking time of the currently clocked in entry *)
