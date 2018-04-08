module type Trie = sig
	
	type key = string
	type value
	type t

	val empty : t
	val insert : t -> key -> value -> t
	val member : key -> t -> bool
	val find : key -> t -> value option
	val begin_with : (value -> bool) -> key -> t -> key list

end