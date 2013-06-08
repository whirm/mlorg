(** Automata for paragraphs *)
(*
This is the first and the simplest automaton. A paragraph is simply a bunch of
lines that end with an empty line.

Well, Actually it's not so simple. To ease things up, 
On interruption we check that the block is (or not a footnote definition)  
*)
open Org_automaton
type state = string list
(* The state : the lines seen so far *)

(* When interrupting this parser, we just parse the inline contents, if the list of
lines is non-empty: *)
let interrupt context lines _ = match List.rev lines with
  (* This case is to avoid cluttering the output tree with empty blocks *)
  | [] -> context, []
  | (head :: tail) as lines ->
      let parse lines = Org_inline.parse (String.concat "\n" lines) in
      (* We check for a footnote definition in the first line *)
      try Scanf.sscanf head "[%[^[][]] %[^\n]" (fun name rest ->
        (* NB: we want to ignore [[... because they are links often *)
        if name.[0] = '[' then failwith "";
        (* If it matches, we return a footnote definition *)
        context, [Block.Footnote_Definition (name, parse (rest :: tail))])
      (* If it does not we return a boring paragraph *)
      with _ -> context, [Block.Paragraph (parse lines)]
        
(* To parse a string, we just check if it's empty. If so we are done. If not, we
   are partially done (can be interrupted). *)
let parse_line lines { line; context } = 
  if line = "" then
    let context, blocks = interrupt context lines () in
    context, Done (blocks, true)
  else context, Partial (line :: lines)

(* To know if we are in the beginning of a paragraph, it's easy: it's always the case ! *)
let is_start { line; context } = Some (context, [line])

(* We are the lowest priority automata. Everyone can interrupt us :-( *)
let priority = 0
