let do_parse cin a =
  let l = input_line cin in
  Scanf.sscanf l a


let analyse_base cin =
  let n = do_parse cin "%d" (fun x -> x) in
  Array.to_list
    (Array.init n
       (fun _ ->
         do_parse cin "%s %s %d" (fun a b c -> a,b,c)
       )
    )

let analyse_file_1 fn =
  let cin = open_in fn in
  let transitions = analyse_base cin in
  let start_stop = do_parse cin "%s %s" (fun a b -> a,b) in
  let _ = close_in cin in
  transitions,start_stop


let output_sol_1 time mod_list =
  Format.printf "%d : %s" time (List.hd mod_list);
  List.iter (Format.printf " -> %s") (List.tl mod_list);
  Format.printf "@."


let split_on_char c s =
  let rec aux i =
    try
      let j = String.index_from s i c in
      String.sub s i (j-i)::aux (j+1)
    with _ -> [String.sub s i (String.length s - i)]
  in
  aux 0

let parse_trajets cin =
  let m = do_parse cin "%d" (fun x -> x) in
  Array.to_list (Array.init m (fun _ ->
      let s = input_line cin in
      List.filter (fun s -> s<>"" && s<> "->") (split_on_char ' ' s)
    ))

let analyse_file_2 fn =
  let cin = open_in fn in
  let transitions = analyse_base cin in
  let trajets = parse_trajets cin in
  transitions,trajets

let parse_trajets_2 cin =
  let m = do_parse cin "%d" (fun x -> x) in
  Array.to_list (Array.init m (fun _ ->
      let s = input_line cin in
      match List.filter (fun s -> s<>"" && s<> "->") (split_on_char ' ' s) with
      | src::dst::[] -> src,dst
      | _ -> raise (Scanf.Scan_failure "format invalide")
    ))


let analyse_file_3 fn =
  let cin = open_in fn in
  let transitions = analyse_base cin in
  let trajets = parse_trajets_2 cin in
  transitions,trajets



let output_sol_2 sol_list =
  List.iter (fun (mvs,sol) ->
      Format.printf "%s" (List.hd mvs);
      List.iter2 (fun n t -> Format.printf " -%d-> %s" t n) (List.tl mvs) sol;
      Format.printf "@."
    )
    sol_list
