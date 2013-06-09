(** Math2png: exports LaTeX formulas as images *)

(** This modules turn some formulas (up to the user) from the document into PNG images. *)

val transform: Config.instance -> Document.t -> Document.t
(** Do the transformation *)
