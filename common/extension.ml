#+INFOJS_OPT:
#+PROPERTY: tangle yes



* Extensions
This module defines the interface that an extension should have:

#+begin_src ocaml
open Prelude
module type Ext = sig
#+end_src

- The name of the extension (in the future, more metadata) :
  #+begin_src ocaml
  val name : string
  #+end_src

- The configuration for this extension. It will be filled by the caller
  by the configuration the user supplied either in the command line, or through directives.
  #+begin_src ocaml
  val config : Config.t
  #+end_src

#+begin_src ocaml
end
#+end_src

=find name list= finds the desired extension in =list=, by its name.
#+begin_src ocaml
  let find name = List.find (fun x -> 
    let module E = (val x : Ext) in
    E.name = name)
  
#+end_src
