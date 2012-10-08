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
