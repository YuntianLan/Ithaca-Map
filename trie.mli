
(* The TRIE module usesthe prefix tree data structure
 * to quickly index the value given the key.
 * Considering the scope of this project, our TRIE would
 * only support insertion of k/v pair and loopup (not remove)
 *)

(* type of values stored in the trie
type value *)
(* type of the Trie *)
type 'a trie

(* [empty] is an empty TRIE *)
val empty : 'a trie

(* [insert t k v] is a new Trie with the key k binding
 * to value v, if k already exists then its old
 * value is replaced. *)
val insert : 'a trie -> string -> 'a -> 'a trie

(* [member t k] returns true iff the key k is in the Trie *)
val memb : 'a trie -> string -> bool

(* [find t k] returns Some v if k exists in t else None
 * The search is case insensitive *)
val find : 'a trie -> string -> 'a option

(* [begin_with t f k] returns the list of all values whose keys
 * begin with the string k and the value satisfies the filter function f. *)
val begin_with : 'a trie -> ('a -> bool) -> string -> 'a list



