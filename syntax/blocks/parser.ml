(** The parser for blocks *)
open Batteries
open Prelude
open Block
open Automaton

(* The main parser for blocks content. The algorithm is the following, given a list
of automata:

1. Search the first automata that accepts the first line
2. Run it until it says it is done (then go back in 1), or if it can be
   interrupted, look for a higher prioritized automaton and interrupt the first,
   to run the second in place and go back to 2.
*)

(**
To be able to manipulate state, we need to wrap them into a module :
*)
module type State = sig
  type state
  module Aut : Automaton.Automaton with type state = state
  val state : state
end
(** A module that holds the current state for the parser.
    Contains the automata and its state *)

(**
The following function returns the first automaton in a list of automaton to
accept the input. It also returns the list of previous candidates. It will be
useful because the list will be sorted by priority decreasing so the list
returned will be the list with higher priority than the one returned.
*)
let faa input = 
  let rec aux acc = function
    | [] -> failwith ("No accepting automaton for " ^ input.line)
    | t :: q -> 
      let module A = (val t : Automaton.Automaton) in
      match A.is_start input with
        | None -> aux (t :: acc) q
        | Some (context, state) -> context, List.rev acc, 
          (module struct type state = A.state
                         let state = state
                         module Aut = A
          end : State)
  in aux []

(**
The following functions are wrapper around the inner functions contained in [State]
*)
let interrupt context v = 
  let module S = (val v : State) in
  S.Aut.interrupt context S.state
    
let parse_line v i = 
  let module S = (val v : State) in
  match S.Aut.parse_line S.state i with
    | x, Automaton.Done _ as c -> c
    | x, Automaton.Next st ->
      x, Automaton.Next (module struct include S let state = st end : State)
    | x, Automaton.Partial st ->
      x, Automaton.Partial (module struct include S let state = st end : State)

(*
In particular, note that parse_line returns a [return] of a modified type.
*)


let rec handle_lines myself lines ({ Context.number } as context_) 
    all previous state = 
  let aux ?context number = 
    handle_lines myself lines 
      { (Option.default context_ context) with Context.number} all
  in
  let context = context_ in
  (* We distinguish cases depending on :
     - whether there is still line to parse
     - whether we have a running automaton
  *)
  match Enum.get lines, state with
    (* First case : empty input, we do not have anything to return *)
    | None, None -> None
    (* Second case : no input, but a running automaton. Interrupt it 
       and append the result to the accumulator *)
    | None, Some st -> 
      let context, blocks = interrupt context st myself in
      Some (context, [], None, blocks)
    (* A line, but no running automata. Use the {!faa} 
       function to elect a new automaton, *if* the line is non-empty.
       Otherwise skip the line *)
    | Some line, None -> 
      if line = "" then
        aux (number + 1) previous None
      else
        let input = { line; context; parse = myself } in
        let context, previous, state = faa input previous in
        aux ~context (number + 1) previous (Some state)

    (* A line, and a running automaton. Give it the line to eat, and act upon its return. *)
    | Some line, Some state ->
      let input = { line; context; 
                    parse = myself } in
      match parse_line state input with
          (* He's done. Whether he digested the line, make the proper modification on lines/number. *)
        | context, Automaton.Done (bl, b) ->
          let number = if not b then (Enum.push lines line; number)
            else number + 1
          in
          Some ({ context with Context.number }, all, None, bl)
        (* He wants to continue. Fine ! *)
        | context, Automaton.Next st' ->
          aux ~context (number + 1) previous (Some st')
        (* He can be interrupted. Look for a potential interruption, or go on *)
        | context, Automaton.Partial st' -> 
          try
            let context, previous', state' = faa input previous in
            let context, blocks = interrupt context state myself in
            Some ({ context with Context.number = number + 1 }, previous, 
                  Some state', blocks)
          with _ ->
            aux (number + 1) previous (Some st')

let rec parse list context lines = 
  let list = Automaton.sort list in
  let myself context lines = parse list context lines in
  let rec aux blocks context previous state = 
    match handle_lines myself lines context list previous state with
      | None -> context, List.rev blocks
      | Some (context, previous, state, blocks') ->
        aux (blocks' @ blocks) context previous state
  in
  aux [] context list None

let parse_lazy list context lines = 
  let with_buffer f =
    let buffer = ref [] in
    let rec g () = match !buffer with
      | t :: q -> buffer := q; Some t
      | [] -> match f () with
          | None -> None
          | Some l -> buffer := l; g ()
    in Enum.from_while g
  in
  let f = 
    let st = ref (Context.default, list, None) in
    let myself context lines = parse list context lines in
    fun () ->
      let context, list, state = !st in
      match handle_lines myself lines context list list state with
        | Some (context, previous, state, blocks) ->
          st := (context, previous, state);
          Some blocks
        | None -> None
  in with_buffer f
  

let automata = [(module Aut_paragraph : Automaton.Automaton);
 (module Aut_heading : Automaton.Automaton);
 (module Aut_list : Automaton.Automaton);
 (module Aut_directive : Automaton.Automaton);
 (module Aut_math : Automaton.Automaton);
 (module Aut_drawers : Automaton.Automaton);
 (module Aut_blocks : Automaton.Automaton);
 (module Aut_table : Automaton.Automaton);
 (module Aut_latex_env : Automaton.Automaton);
 (module Aut_verbatim : Automaton.Automaton);
 (module Aut_hr : Automaton.Automaton)
]
let parse = parse automata Context.default |- snd
let parse_lazy = parse_lazy automata Context.default

