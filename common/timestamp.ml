open Batteries
open Prelude

type date = {
  year: int;
  month: int;
  day: int;
}
  
type time = {
  hour: int;
  min: int;
}
type t = {
  date: date;
  time: time option;
  repetition: date option;
  active : bool;
}
type range = {
  start: t;
  stop: t;
}
let year t = t.date.year
let month t = t.date.month
let day t = t.date.day

let hour t = Option.map_default (fun x -> x.hour) 0 t.time
let min t = Option.map_default (fun x -> x.min) 0 t.time

let hour_opt t = Option.map_default (fun x -> Some x.hour) None t.time
let min_opt t = Option.map_default (fun x -> Some x.min) None t.time

let null_date = { year = 0; month = 0; day = 0 }
let null_time = { min = 0; hour = 0 }
let null = { date = null_date; time = None; repetition = None; active = true }

let to_tm t = 
  let open Unix in
  let tm = {
    tm_sec = 0; tm_min = min t; tm_hour = hour t;
    tm_mday = day t; tm_mon = month t - 1; tm_year = year t - 1900;
    tm_wday = 0; tm_yday = 0; tm_isdst = false
  }
  in snd (mktime tm)
let from_tm ?(active = true) tm = 
  let open Unix in
  let tm = snd (mktime tm) in
  { date = { year = tm.tm_year + 1900; month = tm.tm_mon + 1; day = tm.tm_mday };
    time = Some { hour = tm.tm_hour; min = tm.tm_min };
    active;
    repetition = None }
let normalize t = 
  let normalized = from_tm (to_tm t) in
  let t' = { normalized with repetition = t.repetition } in
  if t.time = None then { t' with time = None } else t'

let weekday t = (to_tm t).Unix.tm_wday

let parse_time s = 
  try
    Scanf.sscanf s "%d:%d" (fun hour min -> Some {hour; min})
  with _ -> None

let parse_date s = 
  try
    Scanf.sscanf s "%d-%d-%d" (fun year month day -> Some {year; month; day})
  with _ -> None
let parse_repetition_marker s = 
  try Scanf.sscanf s "+%d%c" (fun n c -> match c with
    | 'w' -> Some { null_date with day = 7 * n }
    | 'd' -> Some { null_date with day = n }
    | 'm' -> Some { null_date with month = n }
    | 'y' -> Some { null_date with year = n }
    | _ -> None)
  with _ -> None

let parse_timestamp_part timestamp s = 
  let isdigit c = match c with '0' .. '9' -> true | _ -> false in
  if s.[0] = '+' then 
    { timestamp with repetition = parse_repetition_marker s }
  else if isdigit s.[0] then
    { timestamp with time = parse_time s }
  else timestamp

module D = Delimiters.Make (struct
  let table = ['[', (']', false); '<', ('>', false)]
end)
let parse s = 
  if s <> "" && (s.[0] = '[' || s.[0] = '<') then
    match D.enclosing_delimiter (BatSubstring.all s) s.[0] with
      | None -> None
      | Some (parts, rest) ->
          match Prelude.words parts with
            | [] -> None
            | date_s :: rem_parts -> match parse_date date_s with
                | None -> None
                | Some date ->
                    let active = (s.[0] = '<') in
                    let timestamp = List.fold_left parse_timestamp_part
                      { null with date; active }
                      rem_parts
                    in Some (timestamp, BatSubstring.to_string rest)
  else None

let parse_range s = 
  match parse s with
    | None -> None
    | Some (start, rest) ->
        if String.length rest = 0 || rest.[0] <> '-' || rest.[1] <> '-' then 
          None
        else
          match parse (String.sub rest 2 (String.length s - 2)) with
            | Some (stop, rest) ->
                Some ({start; stop}, rest)
            | None -> None

let parse_substring sub = 
  Option.map (fun (x, s) -> x, BatSubstring.of_string s) 
    (parse (BatSubstring.to_string sub))
    
let parse_range_substring sub = 
  Option.map (fun (x, s) -> x, BatSubstring.of_string s) 
    (parse_range (BatSubstring.to_string sub))

let date_to_string d = 
  Printf.sprintf "%d-%2d-%02d" d.year d.month d.day
    
let time_to_string t = 
  Printf.sprintf "%02d:%02d" t.hour t.min

let repetition_to_string d = 
  if d.year <> 0 then Printf.sprintf "+%dy" d.year
  else if d.month <> 0 then Printf.sprintf "+%dy" d.month
  else if d.day <> 0 then Printf.sprintf "+%dd" d.day
  else "+0d"

let to_string ?(wday = [|"sun."; "mon."; "tue."; "wed"; "thu."; "wed."; "sat."|]) t = 
  Printf.sprintf "%c%s%c" 
    (if t.active then '<' else '[')
    ([Some (date_to_string t.date);
      Option.map time_to_string t.time; 
      Option.map repetition_to_string t.repetition;
      Some wday.(weekday t)
     ] |> List.filter_map identity |> String.concat " ")
    (if t.active then '>' else ']')
    
let range_to_string {start; stop} = 
  Printf.sprintf "%s--%s" (to_string start) (to_string stop)

