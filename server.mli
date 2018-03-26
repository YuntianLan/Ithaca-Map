module type MapTree = sig
	type t

	val init_tree : string -> t
	val get_image_dir : float -> float -> int -> string

end

module type MapGraph = sig
	type node
	type edge
	type t

	val init_graph : string -> t

	val get_node_by_coord : float -> float -> t -> node
	val get_node_by_name : string -> node

	val shortest_path : node -> node -> node list

	val node_to_coord : node -> (float * float)

end

module type Server = sig
	type t

	val accept_connection : t -> unit

	val 
	
end




