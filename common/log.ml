
let warning fmt = Printf.kprintf prerr_endline ("Warning:" ^^ fmt)
let error fmt = Printf.kprintf prerr_endline ("Error:" ^^ fmt)
let fatal fmt = Printf.kprintf (fun s -> prerr_endline s; exit 1) ("Fatal:" ^^ fmt)
let info fmt = Printf.kprintf prerr_endline fmt
