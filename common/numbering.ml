open Batteries

type system = { encode : int -> string; 
                decode : string -> int option * string }
let alphabetic_sys alphabet = 
  let lg = String.length alphabet.(0) in
  let base = Array.length alphabet in
  let decode string = 
    let rec aux n k = 
      if k >= String.length string then 
        (if k = 0 then None else Some n), ""
      else
        let s = String.sub string k (String.length string - k) in
        try 
          let n' = Array.findi 
            (fun letter -> String.starts_with s letter) 
                alphabet 
          in
          aux (n * base + n') (k + lg)
        with Not_found -> 
          if k = 0 then None, s
          else Some n, s
    in aux 0 0
  in 
  let encode n = 
    let rec aux acc n = 
      if n < lg then alphabet.(n) :: acc
      else aux (alphabet.(n mod base) :: acc) (n/base)
    in
    String.concat "" (aux [] n)
  in
  { encode; decode }
  
let romain_sys [|one; five; ten; fifty; hundred|] = 
  let encode n = 
    let below_ten [one; five; ten] = function
      | 1 -> [one] | 2 -> [one; one] | 3 -> [one; one; one]
      | 4 -> [one; five] | 5 -> [five] | 6 -> [five; one]
      | 7 -> [five; one; one] | 8 -> [five; one; one; one]
      | 9 -> [one; ten] | 10 -> [ten]
      | _ -> []
    in 
    String.concat ""
      (if n = 49 then [one; fifty]
       else
          (below_ten [ten; fifty; hundred] (n/10)
           @ below_ten [one; five; ten] (n mod 10)))
  in
  let decode s = 
    let map c = List.assoc c
      [one.[0], 1; five.[0], 5; ten.[0], 10; fifty.[0], 50; hundred.[0], 100]
    in
    let rec aux n last_n k =
      if k >= String.length s then 
        (if k = 0 then None else Some n), ""
      else
        try
          let n' = map s.[k] in
          if n' > last_n then
            aux (n - last_n - last_n + n') n' (k+1)
          else
            aux (n + n') n' (k+1)
        with Not_found -> 
          (if k = 0 then None else Some n), String.lchop ~n:k s
    in aux 0 0 0
  in {encode; decode}

module Systems = ExtList.Make (struct
  type t = system
  let base = [
    {encode = string_of_int;
     decode = fun s -> try Scanf.sscanf s "%d%[^\\0]" 
                             (fun n s -> Some n, s)
       with _ -> None, s};
    romain_sys [|"i"; "v"; "x"; "l"; "c"|];
    romain_sys [|"I"; "V"; "X"; "L"; "C"|];
    alphabetic_sys
      [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o";
        "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"|];
    alphabetic_sys
      [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"; "O";
        "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"|];
    alphabetic_sys
      [|"α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ";"μ";"ν";"ξ";"ο";"π";
        "ρ";"σ";"τ";"υ";"φ";"χ";"ψ";"ω"|];
    alphabetic_sys
      [|"Α";"Β";"Γ";"Δ";"Ε";"Ζ";"Θ";"Ι";"Κ";"Λ";"Μ";"Ν";"Ξ";"Ο";"Π";"Τ";
        "Υ";"Φ";"Ψ";"Ω"|];
  ]
end)

let decode s = 
  let rec aux = function
    | [] -> None, s
    | t :: q -> match t.decode s with
        | Some n, s' -> Some (n, t), s'
        | _ -> aux q
  in aux (Systems.get ())

let extract s = 
  let rec aux acc s k = 
    if k >= String.length s then List.rev acc
    else
      match decode (String.lchop ~n:k s) with
        | Some (n, _), s' -> aux (n :: acc) s' 0
        | None, _ -> aux acc s (k+1)
      in aux [] s 0

let update fmt l = 
  let rec aux start len acc k l =
    let rest () = String.sub fmt start len in
    if k >= String.length fmt then 
      List.rev (rest () :: acc)
    else if l = [] then
      List.rev (String.sub fmt start (String.length fmt - start) :: acc)
    else
      match decode (String.lchop ~n:k fmt) with
        | None, _ ->
            aux start (len + 1) acc (k+1) l
        | Some (_, t), s' -> 
            let n = List.hd l and tail = List.tl l in
            let k = String.length fmt - String.length s' in
            aux k 0 (t.encode n :: rest () :: acc) k tail
  in String.concat "" (aux 0 0 [] 0 l)
