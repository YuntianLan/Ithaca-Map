module type ImageTree = sig

	(* [t] is the type of MapTree *)
	type t

	(* [init_tree s] takes in the directory 
	 * where the graphs are stored and builds
	 * the MapTree with all the graphs placed correctly in the 
	 * MapTree. *)
	val init_tree : string -> t

	(* [get_image_dir f f i] takes in the coordinate of 
	 * the map and the zoom level and returns the directory of 
	 * the graph needed. *)
	val get_image_dir : float -> float -> int -> string

end



module type MapGraph = sig

	type node

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
	 * returns the node that corresponds to the building *)
	val get_node_by_name : string -> t -> node

	(* [shortest_path start end] takes in two nodes, one as starting
	 * place and the other as ending place, and returns a node
	 * list which indicates the shortest path betweeen the two
	 * nodes. *)
	val shortest_path : node -> node -> node list

	(* [node_to_coord n] takes in a node and returns the coordinate
	 * of the node. *)
	val node_to_coord : node -> (float * float)

end
