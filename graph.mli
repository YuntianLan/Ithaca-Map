

module type MapGraph = sig

	(* module Tr = StrTrie *)

	type node

	(* [t] is the type of MapGraph *)
	type t

	(* [init_graph s] takes in an json file and returns a MapGraph
	 * with information parsed. *)
	val init_graph : string -> t

	(* [get_node_by_coord lat lon t] takes in an coordinate
	 * and a MapGraph and returns the node that is closest
	 * to the coordinate. *)
	val get_node_by_coord : float -> float -> t -> node

	(* [get_node_by_name s] takes in a building name and
	 * returns the node that corresponds to (encompasses) the building
	 * when the name corresponds to a way of multiple nodes, the first
	 * node in the iteration is returned. *)
	val get_node_by_name : string -> t -> node option

	(* [shortest_path drive s e t] takes in two nodes, one as starting
	 * place and the other as ending place, and returns a tuple of node
	 * list, which indicates the shortest path betweeen the two
	 * nodes, and a float that represents the distance.
	 * The drive flag suggests whether the person is driving. *)
	val find_path : bool -> node -> node -> t -> float * node list

	(* [node_to_coord n] takes in a node and returns the coordinate
	 * of the node, latitude followed by longitude. *)
	val node_to_coord : node -> (float * float)

end




