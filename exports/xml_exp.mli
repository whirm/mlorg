(** Output a document to XML *)

open Config

class xmlExporter : object
  inherit [Xml.t list] Document.bottomUp
  method bot : Xml.t list
  method combine : Xml.t list list -> Xml.t list
  method range : Timestamp.range -> Xml.t
  method time : Timestamp.time -> Xml.t
  method date : Timestamp.date -> Xml.t
  method timestamp : Timestamp.t -> Xml.t
  method meta : Document.t -> Xml.t list
  method property : string * string -> Xml.t
  method footnote : (string * Inline.t list) -> Xml.t

end
(** The object used to export to XML. *)

val export : instance -> Document.t -> unit Batteries.IO.output -> unit
(** [export config document out] exports the document [document] to XML
    and writes the output to [out] *)

