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

		let distance a b g =
			let rec aux l x = match l with
				|[] -> 0
				|a::b -> let (h,j) = a in if h=x then j else aux b x
				in
			let x = NodeMap.find a g in
				let elts = NodeSet.elements x in
					aux elts b
end

module Int : (OrderedType with type t = int) = struct
    type t = int
    let compare = compare
end

module MyStringGraph = Make(String);; (* Graphe dont les noeuds sont des String, avec arcs valués *)

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
