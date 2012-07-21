(* Toc-based operations *)


type item = 
    {name: Inline.t list;
     children: t}
and t = item list
let gather doc = 
  let o = object(self)
    inherit [t] Document.folder 
    method heading list t = 
      let item = {
        name = t.Document.name;
        children = List.rev (List.fold_left self#heading [] t.Document.children)
      }
      in
      item :: list
  end
  in
  o#document [] doc


let rec mem_item name toc = 
  name = Inline.asciis toc.name
  || mem name toc.children
and mem name = List.exists (mem_item name)
