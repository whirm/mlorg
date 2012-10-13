(** Html generation *)
open Config
(** This module deals with html generation for mlorg.
    Generation is done in a pretty straightforward way, returning a {!Xml.t}
 *)

module Config : sig
  val encoding : string Config.item
  val full : bool Config.item
  val style : string Config.item
  val number_lines : bool Config.item
  val use_math2png : bool Config.item
  val image_extensions : string list Config.item
  val use_pygments : bool Config.item
end
(** The configuration item of this module. See the documentation for the meaning
    of them. *)

class htmlExporter : instance -> object
  inherit [Xml.t list] Document.bottomUp
  method bot : Xml.t list
  method fancylink : Document.heading -> Xml.t
  method handle_image_link :
               Inline.url -> string -> Inline.t list -> Xml.t list
  method wrap : Document.t -> Xml.t list -> Xml.t
  method combine : Xml.t list list -> Xml.t list
end
(** The object used to export to Html. *)

val export : instance -> Document.t -> unit Batteries.IO.output -> unit
(** [export config document out] exports the document [document] to HTML
    and writes the output to [out] *)
