open Batteries

let get_meta filename = 
  File.lines_of filename
  |> Org_parser.parse_lazy
  |> Enum.take_while (function
      | Block.Directive _ -> true
      | _ -> false)
  |> Enum.map (function
      | Block.Directive (a, b) -> (a, b)
      | _ -> assert false)
  |> List.of_enum

let modify_file ?config func file = 
  let doc, _ = Document.from_file ?config file in
  Batteries.File.with_file_out file
    (Org.export () (func doc))
