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
  ext_opts : (string * (string * string) list) list;
  (** The options of the extensions *)
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

(** {1 Parsing documents} *)    
val from_chan : string -> BatIO.input -> t
(** From an input (the first argument is the filename) *)
val from_file  : string -> t
(** From a file *)
