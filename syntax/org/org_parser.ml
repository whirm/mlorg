module C = struct
  include Org_context
end
include Parser.Make(Org_context)

 
let automata = Automaton.sort [(module Aut_paragraph : Automaton.Automaton);
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
let parse = parse automata Org_context.default
let parse_lazy = parse_lazy automata Org_context.default
