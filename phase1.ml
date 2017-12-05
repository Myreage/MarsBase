module Vertex =
			struct
				type t = string * string
				let compare (x0,x1) (y0,y1) = match String.compare x0 y0 with
					|0 -> String.compare x1 y1
					|c -> c
			end;;

module Graph = Map.Make(Vertex);;


let rec createGraph l g = match l with
	|[] -> g
	|a::b ->	let (x,y,z) = a in
						let g2 =  Graph.add (x,y) z g in
						createGraph b g2;;

let (liste, (init,last)) = Analyse.analyse_file_1 "1.txt";;
let g = createGraph liste Graph.(empty);;

let test1 = ((Graph.find ("n2","n4") g) == 3);;
let test2 = ((Graph.find ("n1","n2") g) == 1);;

let shortestPathDjikstra
