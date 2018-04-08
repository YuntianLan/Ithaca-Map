module type MapImage = sig

	(* [t] is the type of MapImage *)
  type t

  (* [params] is the type of query parameters for getting the map *)
  type params

  (* [result] is the type of map image information *)
  type result

	(* [init_quadtree] takes in the root image
	 * where the graphs are stored in the same folder and builds
	 * the MapTree with all the graphs placed correctly in the
	 * MapTree. *)
	val init_quadtree : string -> t

  (* [get_map params] is the map image information that corresponds to the query
   * information [params] *)
	val get_map : params -> result

end
