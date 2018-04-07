module type ImageTree = sig

	type t

	val init_tree : string -> t
	val get_image_dir : float -> float -> int -> string

end

module QuadTree : ImageTree = struct
	
	type t = unit

	let init_tree s =
		failwith "Unimplemented"

	let get_image_dir lat lon id = 
	 failwith "Unimplemented"

end