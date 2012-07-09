(** Configuration for modules (Exporters, Transformers, ...) *)

(** This module deals with configurations for modules. The goal of this module
    is provide a way for modules to register an interface for configuration
    items such that there is a difference between an interface and an
    instance. An interface describes the parameter can take, and an instance are
    some values for part of the parameters. The instance will be fed to the
    module when using his functionnality (exporting, ..) so that it is
    reentrant. *)

(** {1 Description} *)
(** {2 Description of the implementation} *)

(** The implementation is as follows. Configuration items are based on
    {!SerializableType}s: types with[show]/[read] functions. Each item has such
    a type, a name, a description, and a default value of this type.
    
    Then an interface is basically a list of those items. A realization is a
    polymorphic function of type 'a. 'a Item -> 'a that for any item returns a
    value of that type. The intuition is that for items on which the instance is
    defined it will return the custom value set by the user, otherwise it
    returns the default value
*)

(** {2 How to use it} *)

(** See the exporters for some example.  Basically you need first to create a
    {!preconfiguration} in which you {!add} your argument. Each subsequent call
    to {!add} will give you an item you should conserve and will be used to
    retrieve the options's value.

    When you added all the options, you create configuration by {!validate}-ing
    your preconfiguration that will optimize the representation for O(1) access
    to variables.
*)

(** {1 Documentation} *)
(** {2 Serializable types} *)

(** This section defines what a serializable type is, and
    basic instances for common types *)
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

(** {2 Configurations} *)

type 'a item
(** An item, that is a configuration entry *)

type preconfiguration
(** A preconfiguration -- something you can add items to. *)

type interface
(** A set of configuration entry. It is definitive, you cannot add anything to it *)

type t = interface
(** Short-hand for interfaces *)

val create : unit -> preconfiguration
(** Creates an empty preconfiguration *)

val add : preconfiguration -> string -> 'a serializable -> string -> 'a -> 'a item
(** [add config name serial description default] adds a new item composed with
    the arguments in the configuration [config]. It returns the created item *)

val validate : preconfiguration -> interface
(** [validate preconfiguration] validates [preconfiguration] and turns it into a proper interface *)

type instance = {get : 'a. 'a item -> 'a}
(** An instance of a configuration -- defining a value of a finite number of item *)

val make : t -> (string * string) list -> instance
(** Make an instance out of a configuration and a few defined values *)

val append : interface -> (string * string) list -> (instance -> instance)
(** [append interface values instance] will create a new instance which will be defined on values
    of [instance] and on values appearing in [values] *)

(** {2 Parsing strings} *)

val parse_comma : string -> (string * string) list
(** Parse a comma-separated keyvalue string : [foo=bar, bar=foo]...*)

val from_comma : interface -> string -> instance
(** Make an instance out of a comma-separated keyvalue string : [foo=bar, bar=foo]...*)

