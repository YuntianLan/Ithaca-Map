(* A [Server] sits on the cloud VM and responds to client
 * requests by looking at its stored tree and graph.
 * The tree and graph are pre-processed immutable data types
 * that provide API for the server to respond to the client.
 *)
module type Server = sig
	

	(* [Tree] is a module representing the directories
	 * of all the images with different resolutions. *)
	module Tree : Image.MapImage

	(* [Graph] is a module representing the relations 
	 * between nodes on the map. *)
	module Graph : Graph.MapGraph


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




