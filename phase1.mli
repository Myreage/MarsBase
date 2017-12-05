module Vertex :
			sig
				type t = string * string
				val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
			end;;
