(** Entity definition *)
(** This short files defines the entities of org.
    Note that latin1 part of entities have been suppressed. *)
type t = {
   name : string;
   latex : string;
   latex_mathp : bool;
   html : string;
   ascii : string;
   unicode : string;
}
type entity = t

module Entities : ExtList.ExtList
(** The list of entities *)

val find : string -> entity
(** [find name] finds the entity named [name] or raises [Not_found] *)
