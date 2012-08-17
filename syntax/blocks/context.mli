(** Context to parse blocks *)
(** This file defines what the context of the parser will be. This is necessary
    because some directives can modify this context along the parsing. *)
type t = {
  markers : string list;
(** The list of markers ([TODO], [DONE], ...) *)
  number : int;
(** The number of the line in the input (Put here because in Parser it can be
    given by the automata) *)
}
(** A context to know about the configuration of the parser *)


val default : t
(** The default context *)
val handle_directive : t -> string * string -> t
(** Handle a directive found in a file to update the configuration *)
