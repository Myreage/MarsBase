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

    let add_edge src dst value g =
        let src_succs =
            try NodeMap.find src g
            with Not_found -> NodeSet.empty in
        let src_succs_with_dist = NodeSet.add (dst,value) src_succs in
        let g2 = NodeMap.add src src_succs_with_dist g in
        add_node dst g2

    let remove_edge src dst g =
        try
            let src_succs = (NodeMap.find src g) in
            let src_succs_without_dist = NodeSet.remove dst src_succs in
            NodeMap.add src src_succs_without_dist g
        with Not_found -> g

    let fold_node f g v0 = NodeMap.fold (fun n n_succs acc -> f n acc) g v0

    let fold_edge f g v0 =
        NodeMap.fold (
            fun src src_succs acc -> NodeSet.fold (fun dst acc_src -> f src dst acc_src) src_succs acc
        ) g v0

    let succs n g = NodeMap.find n g;;
end

module Int : (OrderedType with type t = int) = struct
    type t = int
    let compare = compare
end

module MyStringGraph = Make(String);; (* Graphe dont les noeuds sont des String, avec arcs valués *)

let test = MyStringGraph.add_edge "n1" "n2" 3 MyStringGraph.empty;;
let test = MyStringGraph.add_edge "n2" "n4" 5 test;;
let test = MyStringGraph.add_edge "n1" "n4" 7 test;;

print_string "Nodes : "; print_string (MyStringGraph.fold_node (fun n acc -> (n^" - "^acc)) test "");;
print_string "\nEdges : "; print_string (MyStringGraph.fold_edge (fun src (x,y) acc_src -> (acc_src^" - "^x^" #Value : "^(string_of_int y)^"\n")) test "");;

let (liste, (init,last)) = Analyse.analyse_file_1 "1.txt";;

let rec createGraph l g = match l with
	|[] -> g
	|a::b ->	let (x,y,z) = a in
						let g2 =  MyStringGraph.add_edge x y z g in
						createGraph b g2;;
