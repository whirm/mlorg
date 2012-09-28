(** Automaton specialized to org parser *)

include module type of (Automaton.Make (Org_context))
