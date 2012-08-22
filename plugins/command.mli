(** Running command and capturing output. *)
exception Failed of string * string
(** Exception raised by [run] when the command did not exit successfully (ie. returned 0).
    The first argument is the command that failed, and the second argument the content of stderr *)

val run : ?feed:string list -> string -> string
(** [run command] runs the command [command] and returns its output.
    You can use the optional parameter feed to give the program some input *)

val run_fmt : ?feed:string list -> ('a, unit, string, string) format4 -> 'a
(** [run_fmt] is [run] but expecting a Printf format. *)
