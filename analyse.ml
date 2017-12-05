
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
    
