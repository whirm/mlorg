(* Macro expansions *)
open Prelude
open Plugin
open Inline
open Block
open Config
open Batteries
open Document

module P = struct
  let name = "macros"
  let config = Config.create ()
  type interface = unit
  let data = ()
end
let () = Plugin.General.add (module P : Plugin with type interface = unit)
open P


let substitute_macro doc name arguments = 
  let macros = 
    doc.directives |> List.filter (fst %> String.uppercase %> (=) "MACRO")
      |> List.map (snd %> String.split ~by:" ")
  in
  try List.assoc name doc.directives
  with Not_found ->
    try let value = List.assoc name macros in
        let buff = Buffer.create (String.length value) in
        Buffer.add_substitute buff (fun v -> try
                                               List.nth arguments (int_of_string v - 1)
          with _ -> v);
        Buffer.contents buff
    with Not_found -> Log.warning "Macro %s not found" name; ""


let transform doc = 
  let o = object (self)
    inherit [unit] Document.mapper as super
    method inline () = function
      | Inline.Macro (name, arguments) -> 
        Inline.List (self#inlines () (Org_inline.parse (substitute_macro doc name arguments)))
      | t -> super#inline () t
  end in o#document () doc
