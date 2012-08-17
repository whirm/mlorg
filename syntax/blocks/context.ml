type t = {
  markers : string list;
  number : int;
(** The number of the line in the input *)
}

let default = { markers = ["TODO"; "WAITING"; "DONE"]; number = 1 }
let handle_directive context (name, value) = match name with
  | "TODO" | "SEQ_TODO" | "TYP_TODO" -> 
    { context with markers = Prelude.words value }
  | _ -> context
