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


