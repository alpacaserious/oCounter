open Stdio

(* read all lines *)
let lines = In_channel.input_lines (In_channel.create "log.csv")

let getNames l =
  let split = String.split_on_char ',' l in
  let text = List.nth split 3 in

  (* return None if empty string *)
  if String.length text > 2 then
    (* trim " []" from right *)
    let res =
      let idx = String.index_opt text '[' in
      match idx with Some i -> String.sub text 0 (i - 1) | None -> text
    in
    let r = String.split_on_char ';' res in
    List.map String.trim r
  else []

(* map (getNames) -> list of list of names *)
let names = List.flatten (List.map getNames (List.tl lines))

let count l =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun key ->
      if Hashtbl.mem tbl key then
        Hashtbl.replace tbl key (Hashtbl.find tbl key + 1)
      else Hashtbl.add tbl key 1)
    l;
  Hashtbl.fold (fun k v ls -> (k, v) :: ls) tbl []

let pairs = count names
let sorted = List.sort (fun l r -> String.compare (fst l) (fst r)) pairs
let () = List.iter (fun f -> printf "%d %s\n" (snd f) (fst f)) sorted
