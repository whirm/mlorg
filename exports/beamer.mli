(** Beamer exporter *)

val export : Config.instance -> Document.t -> unit Batteries.IO.output -> unit
(** [export config doc out] exports the document [document] in beamer to [out] with config [config] *)

class beamerExporter : Config.instance -> unit Batteries.IO.output -> object
  inherit Latex.latexExporter
  (** This method doesn't write the header/footer *)
end
(** The exporter class for Beamer *)
