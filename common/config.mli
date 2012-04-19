(** Configuration for modules (Exporters, Transformers, ...) *)

(** This module deals with configurations for modules. It is able to read the
configuration for a specific module in a comma-separated string.
wa (=:foo bar ..=), or ini-style.
*)


(** {1 Serializable types} *)

module type SerializableType = sig
  type t
  val show : t -> string
  val read : string -> t option
  val description : string
end
(** A type that can be read/write from/to a string *)

type 'a serializable = (module SerializableType with type t = 'a)
(** An alias to hide the module *)

(** Some instances of SerializableType for most common types: *)  

val int : int serializable
(** Serializable instance for integers *)

val boolean : bool serializable
(** Serializable instance for booleans ([true]/[false] or [yes]/[no]) *)

val string : string serializable
(** Serializable instance for strings (either [foo] or ["foo with a space"]) *)

val couple : 'a serializable -> 'b serializable -> ('a * 'b) serializable
(** Serializable for couples ([(a, b)]) *)

val list : 'a serializable -> 'a list serializable
(** Serializable for lists ([[a1, a2, ...]]) *)

(** {1 Configurations} *)

module type Item = sig
  type t
  (** The type of the values that the item expect *)
  module T : SerializableType with type t = t
  (** An instance of {!SerializableType} for [t] *)
  val name : string
  (** The item's name. *)
  val description : string
  (** A description for the user of what the item configures *)
  val default : t
  (** The item's default value. *)
  val ref : t ref
  (** The item's current value. *)
end
(** A configuration option (or item).
    Each module will specify a list of those *)

type 'a item = (module Item with type t = 'a)
(** Usual wrapper around {!Item} *)

type t
(** A configuration will denote a list of {!Item}'s *)

val create : unit -> t
(** Creates an empty configuration *)

val add : t -> string -> 'a serializable -> string -> 'a -> 'a item
(** [add config name serial description default] adds a new item composed with
    the arguments in the configuration [config]. It returns the created item *)

val get : 'a item -> 'a
(** Retrieves an item's content *)


(** {1 Parsing strings} *)
val parse_comma : string -> (string * string) list
(** Parse a comma-separated keyvalue string : [foo=bar, bar=foo]...*)
(** {1 Configuration parsing} *)

val fill_comma : t -> string -> unit
(** [fill_comma config s] parses [s] as a key-value pair separated by comma,
    and uses the values to fill the configuration [config]. *)

val fill : t -> (string * string) list -> unit
(** [fill t assoc] fills the configuration [t] using the data in [assoc] *)

val reinit : t -> unit
(** [reinit config] reinits every item to the default. *)
