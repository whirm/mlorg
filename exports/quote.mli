(** Quote export *)


module Dynamic : module type of Dynamic.Make (struct let name = "" type t = Document.t -> unit Batteries.IO.output -> unit end)
(** The entry point for dynamic outputs *)
