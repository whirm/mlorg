(** Interface to pygments *)

val color : Config.instance -> string -> string -> string list -> string
(** [color config lexer formatter lines] invokes pygments on the code denoted by [lines],
    with the output formatter [formatter], and lexer [lexer]. *)

val style_def : ?style: string -> Config.instance -> string -> string
(** [spell_out_style ?style config formatter] invokes pygments to spell out the definition of style [style]
    (default: the style specified in the configuration of the module) for formatter [formatter] *)
