(* The TRIE module usesthe prefix tree data structure
 * to quickly index the value given the key.
 * Considering the scope of this project, our TRIE would
 * only support insertion of k/v pair and loopup (not remove)
 *)
module type Trie = sig
  (* type of values stored in the trie *)
  type value
  (* type of the Trie *)
	type t

	(* [empty] is an empty TRIE *)
	val empty : t

	(* [insert t k v] is a new Trie with the key k binding
	 * to value v, if k already exists then its old
	 * value is replaced. *)
	val insert : t -> string -> value -> t

	(* [member t k] returns true iff the key k is in the Trie *)
	val member : t -> string -> bool

	(* [find t k] returns Some v if k exists in t else None
	 * The search is case insensitive *)
	val find : t -> string -> value option

	(* [begin_with t f k] returns the list of all values whose keys
	 * begin with the string k and the value satisfies the filter function f. *)
	val begin_with : t -> (value -> bool) -> string -> value list

end

(* A [S] is a value that can be stored in the Trie *)
module type S = sig
  (* The value has type t *)
  type t
end

(* [MakeTrie] makes a [Trie] with value [M] *)
module type MakeTrie =
  functor (M : S) -> Trie
