module type MapGraph = sig

	type node
	type way

(* 	type kdtree
	type trie *)



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
	 * returns the nodes that corresponds to (encompasses) the building
	 * *)
	val get_nodes_by_name : string -> t -> node list

	(* [shortest_path s e] takes in two nodes, one as starting
	 * place and the other as ending place, and returns a node
	 * list which indicates the shortest path betweeen the two
	 * nodes. *)
	val shortest_path : node -> node -> node list

	(* [node_to_coord n] takes in a node and returns the coordinate
	 * of the node. *)
	val node_to_coord : node -> (float * float)

end
