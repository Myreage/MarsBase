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
		val voisins : node -> (NodeMap.key * NodeSet.t) list -> (NodeMap.key * NodeSet.t) list
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
				let elt = NodeSet.elements x in
					aux elt b

			let is_node_in_set n set =
				NodeSet.exists (fun x -> let (a,b) = x in
				if (a=n) then true else false) set


			let rec voisins a nodes_list = match nodes_list with
				|[]-> []
				|(x,y)::t -> 	if (is_node_in_set a y) then (x,y)::(voisins a t)
											else voisins a t;;
end


module MyStringGraph = Make(String);; (* Graphe dont les noeuds sont des String, avec arcs valués et sans direction *)
module DijkstraMap = Map.Make(String);;


let (liste, (init,last)) = Analyse.analyse_file_1 "1.txt";;

let rec createGraph l g = match l with
	|[] -> g
	|a::b ->	let (x,y,z) = a in
						let g2 =  MyStringGraph.add_edge x y z g in
						createGraph b g2;;

let g = createGraph liste MyStringGraph.empty;;

let rec init_dijkstramap map nodes start = match nodes with
		|[] -> map
		|(x,y)::t -> 	if x=start then init_dijkstramap (DijkstraMap.add x (0,false,"") map) t start
									else init_dijkstramap (DijkstraMap.add x (-1,false,"") map) t start;;

let find_min dijkstramap = (* renvoie (min_key,distance) *)
	DijkstraMap.fold (fun k v acc ->	let (w,_,_) = v in
											let (x,w') = acc in
											if (w<w' || w'=(-1)) then (k,w) else acc)
	dijkstramap ("",-1);;

let find_min_unseen dijkstramap =
	DijkstraMap.fold (fun k v acc ->	let (w,z,t) = v in
											let (x,w',_,_) = acc in
											if ((w<w' || w'=(-1)) && (not z) && (w!=(-1))) then (k,w,z,t) else acc)
	dijkstramap ("",-1,false,"");;


let rec update dijkstramap voisins node_curr g = (* voisins = [(a,[..]),..] *)
let (a,b,_,d) = node_curr in
match voisins with
	|[] -> dijkstramap
	|(x,l)::t -> 	let dist = MyStringGraph.distance x a g in
								let (dist_fils,c,_) = DijkstraMap.find x dijkstramap in
								if ((((b + dist) < dist_fils) || (dist_fils = -1)) && (not c))
								then
								update (DijkstraMap.add x (b + dist, c,a) dijkstramap) t node_curr g
								else
								update dijkstramap t node_curr g;;

let rec dijkstra_aux dijkstramap node_curr nodes g e =
	let (a,b,_,d) = node_curr in
	let dijkstramap = DijkstraMap.add a (b,true,d) dijkstramap in
	let voisins = MyStringGraph.voisins a nodes in
	let dijkstramap = update dijkstramap voisins node_curr g in
	let (min,_) = find_min dijkstramap in
	if (min=e) then dijkstramap
	else dijkstra_aux dijkstramap (find_min_unseen dijkstramap) nodes g e;;

let rec path dijkstramap s e =
	if (e=s) then []
	else let (t,_,d) = 	DijkstraMap.find e dijkstramap
	 										in d::(path dijkstramap s d);;

let dijkstra g start e =
	let nodes = MyStringGraph.bindings g in (* nodes est de la forme [("ni",[("nj",3),("nk", 6),...]),...]  *)
	let dijkstramap = init_dijkstramap DijkstraMap.empty nodes start in (* distances est de la forme [(x,w,z),...] avec x noeud, w poids et z indicateur de visite *)
	let dijkstramap = dijkstra_aux dijkstramap (start,0,false,"") nodes g start in
	let res = path dijkstramap start e in
	let (t,_,_) = DijkstraMap.find e dijkstramap in
	(t,List.rev (e::res));;

let (t,l) = dijkstra g "n1" "n8" in
output_sol_1 t l;;

(* PHASE 2 *)
module StringPair = struct
	type t = string*string
	let compare (x1,y1) (x2,y2) = match (String.compare x1 x2) with
		|0 -> String.compare y1 y2
		|c -> c;;
end

module Int = struct
	type t = int
	let compare = compare
end

module Ph2Map = Map.Make(StringPair);;
module Ph2Res = Map.Make(Int);;

let (arcs, paths) = Analyse.analyse_file_2 "2.txt";;

let rec initph2map_arcs arcs = match arcs with
	|[] -> Ph2Map.empty
	|(x,y,z)::b -> Ph2Map.add (x,y) (z,0) (Ph2Map.add (y,x) (z,0) (initph2map_arcs b));;

let rec initph2map_res path i = match path with
	|[] -> Ph2Res.empty
	|a::b -> Ph2Res.add i (a,[],false,("","")) (initph2map_res b (i+1));;


(* algo
1. 	l = nombre de paths (=nbr d'users)
2. 	tant que tous les paths ne sont pas vides:
			temps ++
			pour tous les users:
				cas  user en transition: si temps dans le tunnel - 1 = 0 -> user plus en transition / sinon temps -1
				cas user pas en transition : on le fait avancer si l'arrête est libre, sinon il attend
*)
let rec ph2_move user path  map mapres time =	(*update pour un user à un temps donné*)
	let (p,l,t,x) = Ph2Res.find user mapres in	(*renvoie (map,mapres)*)
	if t then let (u,v) = Ph2Map.find x map in
		if ((v-1)=0) then ph2_move user path (Ph2Map.add x (u,0) map) (Ph2Res.add user (p,l,false,("","")) mapres) time
		else ((Ph2Map.add x (u,(v-1)) map),mapres)
	else
		match path with
		|[] -> (map,mapres)
		|[a] -> (map, (Ph2Res.add user ([],l,false,("","")) mapres))
		|(a::b::q) ->
		let (u,v) = Ph2Map.find (a,b) map in
		if (v!=0) then (map,mapres)
		else ((Ph2Map.add (a,b) (u,u) map), (Ph2Res.add user ((b::q),((a,b,time)::l),true,(a,b)) mapres));;


let check_end resmap =
	let t = Ph2Res.exists (fun key (l,_,_,_) -> if (l!=[]) then true else false) resmap in
	not t;;

let rec ph2_aux arcsmap resmap i time =
	if (check_end resmap) then (arcsmap,resmap)
	else
	let n = Ph2Res.cardinal resmap in
	if (i=n) then ph2_aux arcsmap resmap 0 (time+1)
	else let (u,_,_,_) = (Ph2Res.find i resmap) in
				let (x,y) =  ph2_move i u arcsmap resmap time in
				ph2_aux x y (i+1) time;;

let ph2 arcs paths =
	let arcsmap = initph2map_arcs arcs in
	let resmap = initph2map_res paths 0 in
	let (x,y) = ph2_aux arcsmap resmap 0 0 in
	Ph2Res.bindings y;;

let _ = ph2 arcs paths;;
