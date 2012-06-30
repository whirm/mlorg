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

type 'a item
(** An item, that is a configuration entry *)

type preconfig
(** A preconfiguration -- something you can add items to. *)

type t
(** A set of configuration entry. It is definitive, you cannot add anything to it *)

val create : unit -> preconfig
(** Creates an empty preconfiguration *)

val add : preconfig -> string -> 'a serializable -> string -> 'a -> 'a item
(** [add config name serial description default] adds a new item composed with
    the arguments in the configuration [config]. It returns the created item *)

val validate : preconfig -> t
(** Make the preconfiguration definitive *)

type instance = {get : 'a. 'a item -> 'a}
(** An instance of a configuration -- defining a value of a finite number of item *)

(** {1 Configuration parsing} *)
val make : t -> (string * string) list -> instance
(** Make an instance out of a configuration and a few defined values *)

(** {1 Parsing strings} *)
val from_comma : t -> string -> instance
(** Make an instance out of a comma-separated keyvalue string : [foo=bar, bar=foo]...*)
val parse_comma : string -> (string * string) list
(** Parse a comma-separated keyvalue string : [foo=bar, bar=foo]...*)
