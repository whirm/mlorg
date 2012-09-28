type context = {
  markers : string list;
  number : int;
(** The number of the line in the input *)
}

type t = context

let default = { markers = ["TODO"; "WAITING"; "DONE"]; number = 1 }
let handle_directive context (name, value) = match name with
  | "TODO" | "SEQ_TODO" | "TYP_TODO" -> 
    { context with markers = Prelude.words value }
  | _ -> context

let number x = x.number
let set_number x y = { x with number = y }
