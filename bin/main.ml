open Stdio

let file = "log.csv"

(* read all lines *)
let lines = In_channel.input_lines (In_channel.create file)

let trimNote l =
    let split = String.split_on_char ',' l in
    let last = List.nth split 3 in

    (* return None if empty string *)
    if String.length last > 2 then
        (* trim " []" from right *)
        let res = if String.contains last '[' then
            let idx = String.index last '[' in
            String.sub last 0 (idx - 1)
        else
            last
        in

        (* TODO: split several names by ';'*)
        Some(res)
    else
        None

(* lines.tl to skip csv column name *)
let notes = List.map trimNote (List.tl lines)

let unwrapped = List.filter_map (fun n -> n) notes

let count l =
    let hash = Hashtbl.create 10 in
    List.iter (fun key -> if Hashtbl.mem hash key then
        Hashtbl.replace hash key ((Hashtbl.find hash key) + 1)
    else
        Hashtbl.add hash key 1) l;
    Hashtbl.fold (fun k v ls -> (k, v) :: ls) hash []

let final = count unwrapped
let () = List.iter (fun f -> printf("%d %s\n") (snd f) (fst f)) final
