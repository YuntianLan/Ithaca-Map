(* reference: https://www.lri.fr/~filliatr/ftp/ocaml/ds/trie.ml.html *)

module type Trie = sig

	type value
	type t

	val empty : t
	val insert : t -> string -> value -> t
	val member : t -> string -> bool
	val find : t -> string -> value option
	val begin_with : t -> (value -> bool) -> string -> value list

end

module type S = sig
  type t
end

module type MakeTrie =
  functor (M : S) -> Trie

module MakeTrie = functor (M:S) -> struct
  type key = string
  type value = M.t

  (* AF: the Node(Some v, [(c1, Node(v1, [sub1])), (c2,Node(v2, [sub2])),...,(cn,Node(vn, [subn]))])
   * represents the Trie with root vaule v, and children with keys [c1...cn] associated to
   * [v1...vn] respectively. None represents that the node has no value or the key is not
   * end of a word
   * RI: There are no duplicate keys in the Trie *)
  type t = Node of value option * (char * t) list

  let to_charlist s =
    let len = String.length s in
    let rec helper i acc =
      if i < 0 then acc
      else helper (i-1) (s.[i]::acc)
    in
    helper (len-1) []

  let empty = Node (None, [])

  (* [find_helper t ks] is the sub-trie that has key matched with [ks] in [t] *)
  let rec find_helper (trie:t) (ks:char list) : t =
    match (trie, ks) with
    | n, [] -> n
    | Node (_, children), h::t ->
      try
        find_helper (snd (List.find (fun child -> Char.lowercase_ascii (fst child) = h) children)) t
      with
      | Not_found -> empty

  let find (trie:t) (s:string) : value option =
    match find_helper trie (s |> String.lowercase_ascii |> to_charlist) with
    | Node (None, _) -> None
    | Node (v,_) -> v


  let member (trie:t) (s:string) : bool =
    match find trie s with
    | None -> false
    | _ -> true

  (* [insert_helper t ks v] is the trie [t] with key value pair [ks],[v] inserted *)
  let rec insert_helper (trie:t) (ks:char list) (v:value) : t =
    match (trie, ks) with
    | Node (_,children), [] -> Node (Some v, children)
    | Node (value, children), h::t ->
      let t' = try
        snd (List.find (fun child -> fst child = h) children)
      with
      | Not_found -> empty in
      let t'' = insert_helper t' t v in
      let new_children = (h, t'')::List.filter (fun c -> fst c <> h) children in
      Node (value, new_children)

  let insert (trie:t) (s:string) (v:value) : t =
    insert_helper trie (to_charlist s) v

  (* [get_all_nodes t f acc] is the list of all values that satisfies the predicate
   * [f] in [t] and [acc] *)
  let rec get_all_nodes (trie:t) (f:value -> bool) (acc:value list):value list =
    let fold_f acc' i =
      get_all_nodes (snd i) f acc' in
    match trie with
    | Node(None, []) -> acc
    | Node(Some v, []) -> if f v then v::acc else acc
    | Node(None, children) ->
      List.fold_left fold_f acc children
    | Node(Some v, children) ->
      if f v then List.fold_left fold_f (v::acc) children
      else
        List.fold_left fold_f acc children


  let begin_with (trie:t) (f:value -> bool) (s:string) : value list =
    let found = find_helper trie (s |> String.lowercase_ascii |> to_charlist) in
    get_all_nodes found f []

end

