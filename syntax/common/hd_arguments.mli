(** Header arguments for code blocks *)

(** This module defines all the possible header arguments in source
    code.  See
    http://orgmode.org/manual/Specific-header-arguments.html#Specific-header-arguments
    for more information *)


type value = 
| Value of string
(** Plain string (NB: It might be a reference) *)
| Call of string * (string * value) list
(** A call [f(x1=a1, ..., xn=an)] *)


(** A var, used to specify the value of variable *)
and t = {
  arguments: (string * string list) list;
  vars: (string * value option) list;
  collection: [ `Value | `Output ];
  typ: [ `Table | `List | `Scalar | `FileLink] option;
  format: [ `Raw | `Org | `Html | `Latex | `Code | `PP | `Drawer ] option;
  output: [ `Silent | `Replace | `Append | `Prepend ];
  file: string option;
  filedescr: string option;
  dir: string option;
  exports: [ `Code | `Results ] list;
  tangle: [ `Yes | `No | `File of string ];
  mkdirp: bool;
  comments: [ `No | `Link | `Yes | `Org | `Both | `Noweb ];
  padline: bool;
  expand: bool;
  session: string option;
  noweb: [ `No | `Yes | `Tangle | `No_export | `Strip_export | `Eval ];
  noweb_ref: string option;
  noweb_sep: string;
  cache: bool;
  sep: string option;
  hlines: bool;
  colnames: bool option;
  rownames: bool;
  shebang: string option;
  eval: [ `No | `Query | `Yes];
  wrap: string;
}

val parse_var : BatSubstring.t -> value * BatSubstring.t
(** Parse a var and returns *)

val parse: string -> t
(** Parse the header arguments of a code *)

val to_string : t -> string
(** Outputs arguments to string *)
