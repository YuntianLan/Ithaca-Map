module type MapTree = sig

	(* [t] is the type of MapTree *)
	type t

	(* [init_tree s] takes in the directory 
	 * where the graphs are stored and builds
	 * the MapTree with all the graphs placed correctly n the 
	 * MapTree. *)
	val init_tree : string -> t

	(* [get_image_dir f f i] takes in the coordinate of 
	 * the map and the zoom level and returns the directory of 
	 * the graph needed. *)
	val get_image_dir : float -> float -> int -> string

end

module type MapGraph = sig

	(* [node] is the type of node in the graph *)
	type node

	(* [edge] is the type of edge in the graph *)
	type edge

	(* [t] is the type of MapGraph *)
	type t

	(* [init_graph s] takes in an xml and returns a MapGraph
	 * with nodes and edges parsed. *)
	val init_graph : string -> t

	(* [get_node_by_coord f f t] takes in an coordinate
	 * and a MapGraph and returns the node that is closest
	 * to the coordinate. *)
	val get_node_by_coord : float -> float -> t -> node

	(* [get_node_by_name s] takes in a building name and
	 * returns the node that corresponds to the building *)
	val get_node_by_name : string -> node

	(* [shortest_path n n] takes in two nodes, one as starting
	 * place and the other as ending place, and returns a node
	 * list which indicates the shortest path betweeen the two
	 * nodes. *)
	val shortest_path : node -> node -> node list

	(* [node_to_coord n] takes in a node and returns the coordinate
	 * of the node. *)
	val node_to_coord : node -> (float * float)

end

(* A [Server] sits on the cloud VM and responds to client
 * requests by looking at its stored tree and graph.
 * The tree and graph are pre-processed immutable data types
 * that provide API for the server to respond to the client.
 *)
module type Server = sig
	

	(* [Tree] is a module representing the directories
	 * of all the images with different resolutions. *)
	module Tree : MapTree

	(* [Graph] is a module representing the relations 
	 * between nodes on the map. *)
	module Graph : MapGraph


	(* [tree] is the type of MapTree
	 * and is a synonym for [Tree.t]. *)
	type tree = Tree.t

	(* [graph] is the type of MapGraph
	 * and is a synonym for [Tree.t]. *)
	type graph = Graph.t

	(* [t] is the type of server *)
	type t

	(* [init_server t g] takes in the pre-processed tree
	 * and graph and builds the server. *)
	val init_server : tree -> graph -> t

	(* [accept_connection t] makes the server to start
	 * listening to client requests continuously
	 * until interrupted. *)
	val accept_connection : t -> unit


end




