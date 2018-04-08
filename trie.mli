(* The TRIE module usesthe prefix tree data structure
 * to quickly index the value given the key.
 * Considering the scope of this project, our TRIE would
 * only support insertion of k/v pair and loopup (not remove)
 *)
module type Trie = sig
	
	type key = string
	type value
	type t

	(* [empty] is an empty TRIE *)
	val empty : t

	(* [insert t k v] is a new Trie with the key k binding
	 * to value v, if k already exists then its old
	 * value is replaced. *)
	val insert : t -> key -> value -> t

	(* [member k t] returns true iff the key k is in the Trie *)
	val member : key -> t -> bool

	(* [find k t] returns Some v if k exists in t else None
	 * The search is case insensitive *)
	val find : key -> t -> value option

	(* [begin_with f k t] returns the list of all keys (string) 
	 * that begin with the string k and whose associated value
	 * satisfies the filter function f. *)
	val begin_with : (value -> bool) -> key -> t -> key list

end














