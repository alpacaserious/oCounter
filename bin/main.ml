open Stdio

(* read all lines *)
let lines = In_channel.input_lines (In_channel.create "log.csv")

let getNames l =
  let split = String.split_on_char ',' l in
  let text = List.nth split 3 in

  (* return None if empty string *)
  if String.length text < 2 then []
  else
    (* trim " []" from right *)
    let res =
      let idx = String.index_opt text '[' in
      match idx with Some i -> String.sub text 0 (i - 1) | None -> text
    in
    List.map String.trim (String.split_on_char ';' res)

(* map (getNames) -> list of list of names *)
let names = List.flatten (List.map getNames (List.tl lines))

let count l =
  let tbl = Hashtbl.create (List.length l / 2) in
  List.iter
    (fun key ->
      if Hashtbl.mem tbl key then
        Hashtbl.replace tbl key (Hashtbl.find tbl key + 1)
      else Hashtbl.add tbl key 1)
    l;
  Hashtbl.fold (fun k v ls -> (k, v) :: ls) tbl []

let pairs = count names

let sort pairs num =
  (* always sort alphabetically, if num then also sort numerically *)
  let list = List.sort (fun l r -> String.compare (fst l) (fst r)) pairs in
  if num then List.sort (fun l r -> Int.compare (snd l) (snd r)) list else list

(* default is numeric *)
let num =
  if Array.length Sys.argv != 2 then true
  else if Sys.argv.(1) = "a" then false
  else true

let sorted = sort pairs num
let () = List.iter (fun f -> printf "%d %s\n" (snd f) (fst f)) sorted
