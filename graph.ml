open Analyse;;

module type OrderedType = sig
    type t
    val compare : t -> t -> int
  end

  (* Signature des graphes *)
  module type MyGraph = sig
    type node
    module NodeSet : Set.S with type elt = node*int
    module NodeMap : Map.S with type key = node
    type graph

    val empty : graph
    val is_empty : graph -> bool

    val add_node : node -> graph -> graph

    val add_edge : node -> node -> int -> graph -> graph

    val fold_node : (node -> 'a -> 'a) -> graph -> 'a -> 'a
    val fold_edge : (node -> node*int -> 'a -> 'a) -> graph -> 'a -> 'a

    val succs : node -> graph -> NodeSet.t

		val distance : node -> node -> graph -> int
		val bindings : graph -> (NodeMap.key * NodeSet.t) list
		val voisins : node -> node list -> node list
		val is_node_in_set : node -> NodeSet.t -> bool

  end

(* Foncteur pour faire des graphes *)
module Make (X : OrderedType) : (MyGraph with type node = X.t) = struct
    type node = X.t

    module Y = struct
        type t = X.t*int
        let compare (a,x) (b,y) = compare x y
    end

    module NodeSet = Set.Make(Y)
    module NodeMap = Map.Make(X)
    type graph = NodeSet.t NodeMap.t

    let empty = NodeMap.empty
    let is_empty g = NodeMap.is_empty g

    let add_node n g =
        if NodeMap.mem n g then g
        else NodeMap.add n NodeSet.empty g

		let find n g = NodeMap.find n g

    let add_edge a b value g =
        let a_succs =
            try NodeMap.find a g
            with Not_found -> NodeSet.empty in
					let b_succs =
		        try NodeMap.find b g
		        with Not_found -> NodeSet.empty in
        let a_succs_with_dist = NodeSet.add (b,value) a_succs in
				let b_succs_with_dist = NodeSet.add (a,value) b_succs in
        let g = NodeMap.add a a_succs_with_dist g in
				let g = NodeMap.add b b_succs_with_dist g in
        let g = add_node b g in
				add_node b g

    let fold_node f g v0 = NodeMap.fold (fun n n_succs acc -> f n acc) g v0

    let fold_edge f g v0 =
        NodeMap.fold (
            fun a a_succs acc -> NodeSet.fold (fun b acc_a -> f a b acc_a) a_succs acc
        ) g v0

    let succs n g = NodeMap.find n g
		let bindings g = NodeMap.bindings g

		let distance a b g =
			let rec aux l x = match l with
				|[] -> 0
				|a::b -> let (h,j) = a in if h=x then j else aux b x
				in
			let x = NodeMap.find a g in
				let elts = NodeSet.elements x in
					aux elts b

			let rec is_node_in_set n set = match set with
				|[] -> 0
				|(x,y)::t -> if x=n then 1 else is_node_in_set n t;;

			let rec voisins a nodes_list = match nodes_list with
				|[]-> []
				|(x,y)::t -> if is_node_in_set a y then (x,y)::(voisins a t);;
end

module Int : (OrderedType with type t = int) = struct
    type t = int
    let compare = compare
end

module MyStringGraph = Make(String);; (* Graphe dont les noeuds sont des String, avec arcs valués et sans direction *)

let (liste, (init,last)) = Analyse.analyse_file_1 "1.txt";;

let rec createGraph l g = match l with
	|[] -> g
	|a::b ->	let (x,y,z) = a in
						let g2 =  MyStringGraph.add_edge x y z g in
						createGraph b g2;;

let g = createGraph liste MyStringGraph.empty;;
print_string "Nodes : "; print_string (MyStringGraph.fold_node (fun n acc -> (n^" - "^acc)) g "");;
print_string "Edges : \n"; print_string (MyStringGraph.fold_edge (fun a (d1,d2) acc_a -> (acc_a^a^" - "^d1^" #"^(string_of_int d2)^"\n")) g "");;


(* DJIKSTRA*)
print_string (string_of_int (MyStringGraph.distance "n2" "n4" g));;

let rec init_distances nodes start = match nodes with
		|[] -> []
		|(x,y)::t -> 	if x=start then 0::(init_distances t start)
									else 999999::(init_distances t start);;

let rec find_index_distances x nodes i = match nodes with
	| [] -> 0
	| (a,b)::t -> if x = b then i else find_index_distances x t i+1;;

let rec find_min nodes_dijkstra nodes distances mini sommet i = match nodes with
	|[] -> sommet
	|(x,y)::t -> if ((distances.[i] < mini) && not(List.mem (x,y) nodes_dijkstra))
	 						 then (find_min nodes_dijkstra t distances distances.[i] x i+1)
							 else (find_min nodes_dijkstra t distances mini sommet i+1);;

let update_distances a b (distances : int list) nodes g predecereurs =
	let ia = find_index_distances a nodes 0 in
	let ib = find_index_distances b nodes 0 in
	let poidsab = MyStringGraph.distance a b g in
	let pred = predecereurs in
	let new_dist = distances in
	if (List.nth distances ib) > ((List.nth distances ia) + poidsab) then
	new_dist.[ib] = distances.[ia] + poidsab;
	pred.[ib] = pred.[ia];
	(new_dist,pred);;

let b = MyStringGraph.bindings g;;
let distances = init_distances (MyStringGraph.bindings g) "n1";;

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let dijkstra g start =

	let rec aux nodes distances = match nodes with (*P résultat*)
	|[] -> distances
	|(x,y)::t ->	let Q = List.filter (fun x -> x=)
	 							let min = find_min Q nodes distances 99999 "" 0
		in

		let rec aux2 voisins = match voisins with
		|[] -> distances
		|(a,b)::t -> aux2 t update_distances min (a,b) distances nodes g predecereurs
		in
		let distances2 = aux2 MyStringGraph.voisins min nodes (* renvoie les nouvelles distances*)
		in

		aux



	let predecereurs = [] in
	let nodes = MyStringGraph.bindings g in
	let distances = init_distances nodes start in
	let res = aux [] nodes nodes;;
