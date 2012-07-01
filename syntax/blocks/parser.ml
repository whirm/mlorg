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
    | [] -> failwith "No accepting automaton."
    | t :: q -> 
      let module A = (val t : Automaton.Automaton) in
      match A.is_start input with
        | None -> aux (t :: acc) q
        | Some state -> List.rev acc, 
          (module struct type state = A.state
                         let state = state
                         module Aut = A
          end : State)
  in aux []

(**
The following functions are wrapper around the inner functions contained in [State]
*)
let interrupt v = 
  let module S = (val v : State) in
  S.Aut.interrupt S.state
    
let parse_line v i = 
  let module S = (val v : State) in
  match S.Aut.parse_line S.state i with
    | Automaton.Done _ as c -> c
    | Automaton.Next st ->
      Automaton.Next (module struct include S let state = st end : State)
    | Automaton.Partial st ->
      Automaton.Partial (module struct include S let state = st end : State)

(*
In particular, note that parse_line returns a [return] of a modified type.
*)


let rec parse number list context lines = 
  let list = Automaton.sort list in
  let myself ?linenumber x = 
    let number = Option.default number linenumber in
    parse number list context x
  in
  let rec aux blocks (previous, state) = 
    (* We distinguish cases depending on :
       - whether there is still line to parse
       - whether we have a running automaton
    *)
    match Enum.get lines, state with
      (* First case : empty input, no running automaton: just return the accumulator *)
      | None, None -> List.rev blocks
  (* Second case : no input, but a running automaton. Interrupt it 
     and append the result to the accumulator *)
      | None, Some st -> List.rev (interrupt st myself @ blocks)
  (* A line, but no running automata. Use the {!faa} 
     function to elect a new automaton, *if* the line is non-empty.
     Otherwise skip the line *)
      | Some line, None -> 
        if line = "" then (incr number; aux blocks (previous, None))
        else
          (let input = { line; number = !number; context; 
                         parse = myself } in
           let previous, state = faa input list in
           incr number;
           aux blocks (previous, Some state))
            
      (* A line, and a running automaton. Give it the line to eat, and act upon its return. *)
      | Some line, Some state ->
        let input = { line; number = !number; context; 
                      parse = myself } in
        incr number;
        match parse_line state input with
          (* He's done. Whether he digested the line, make the proper modification on lines/number. *)
          | Automaton.Done (bl, b) ->
            if not b then (Enum.push lines line; decr number);
            aux (bl @ blocks) (list, None)

          (* He wants to continue. Fine ! *)
          | Automaton.Next st' ->
            aux blocks (previous, Some st')
          (* He can be interrupted. Look for a potential interruption, or go on *)
          | Automaton.Partial st' -> 
            try
              let previous', state' = faa input previous in
              aux (interrupt state myself @ blocks) 
                (previous', Some state')
            with _ ->
              aux blocks (previous, Some st')

    in
    aux [] (list, None)


let parse = parse (ref 0) [(module Aut_paragraph : Automaton.Automaton);
 (module Aut_heading : Automaton.Automaton);
 (module Aut_list : Automaton.Automaton);
 (module Aut_directive : Automaton.Automaton);
 (module Aut_math : Automaton.Automaton);
 (module Aut_drawers : Automaton.Automaton);
 (module Aut_blocks : Automaton.Automaton);
 (module Aut_name : Automaton.Automaton);
 (module Aut_table : Automaton.Automaton)
] Context.default

