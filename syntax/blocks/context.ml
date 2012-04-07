type t = {
  markers : string list;
}

let default = { markers = ["TODO"; "WAITING"; "DONE"] }
let handle_directive context (name, value) = match name with
  | "TODO" | "SEQ_TODO" | "TYP_TODO" -> 
    { context with markers = Prelude.words value }
  | _ -> context
