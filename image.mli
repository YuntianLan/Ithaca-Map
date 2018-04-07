module type MapImage = sig

	(* [t] is the type of MapImage *)
  type t

  (* [params] is the type of query parameters for getting the map *)
  type params

  (* [result] is the type of map image information *)
  type result

	(* [init] takes in the directory
	 * where the graphs are stored and builds
	 * the MapTree with all the graphs placed correctly in the
	 * MapTree. *)
	val init : string -> t

  (* [get_map params] is the map image information that corresponds to the query
   * information [params] *)
	val get_map : params -> result

end
