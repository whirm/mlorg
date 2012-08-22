open Prelude
open Batteries

exception Failed of string * string

let run ?(feed = []) command = 
  let (stdout, stdin, stderr) = Unix.open_process_full
    ~autoclose: true ~cleanup: true command (Unix.environment ())
  in
  List.iter (IO.write_line stdin) feed;
  let stdout_c = IO.read_all stdout
  and stderr_c = IO.read_all stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
    | Unix.WEXITED 0 -> stdout_c
    | _ -> raise (Failed (command, stderr_c))

let run_fmt ?feed fmt = 
  Printf.kprintf (run ?feed) fmt
