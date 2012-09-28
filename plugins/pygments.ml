open Prelude
open Batteries
open Config
open Plugin
type option = 
  | Lineno (** Write line number *)
  | Other of string * string (** Custom options *)

module P = struct
  let name = "pygments"
  type interface = unit
  let data = ()

  let config = Config.create ()
  let binary = Config.add config "command" string "The command to run to invoke pygments"
    "pygmentize -f $formatter -l $lexer -O $options"
    ~vars: [
      make_var "formatter" "the name of the output format to use (eg. latex or html)";
      make_var "lexer" "the name of the input language (eg. ocaml)";
    ]
        
  let style = Config.add config "style" string "The style for pygments to use" "default"
  let spell_out_style = Config.add config "style-spell-out" string 
    "The command to run to spell out the style definitions for a given formatter"
    "pygmentize -S $style -f $formatter"
    ~vars: [make_var "style" "The style which to spell out the definition for";
            make_var "formatter" "Spell out the definition in this formatter"]
end
let () = Plugin.General.add (module P : Plugin with type interface = unit)

let string_of_option = function
  | Lineno -> "linenos=1"
  | Other (a, b) -> Printf.sprintf "%s=%s" a b

let color ?(options = []) config lexer formatter lines = 
  let options_string = String.concat "," (List.map string_of_option options) in
  let command = substitute (flip List.assoc ["formatter", formatter;
                                             "options", options_string;
                                             "lexer", lexer])
    (Config.get config P.binary)
  in
  Command.run ~feed: lines command

let style_def ?style config formatter = 
  let style = Option.default (Config.get config P.style) style in
  let command = substitute (flip List.assoc ["formatter", formatter;
                                             "style", style])
    (Config.get config P.spell_out_style)
  in
  Command.run command
