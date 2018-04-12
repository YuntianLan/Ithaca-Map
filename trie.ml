(* reference: https://www.lri.fr/~filliatr/ftp/ocaml/ds/trie.ml.html *)
module type Trie = sig

	type value
	type t

	val empty : t
	val insert : t -> string -> value -> t
	val member : t -> string -> bool
	val find : t -> string -> value option
	val begin_with : t -> (value -> bool) -> string -> string list

end

module type S = sig
  type t
end

module type MakeTrie =
  functor (M : S) -> Trie

module MakeTrie = functor (M:S) -> struct
  type key = string
  type value = M.t
  type t = Node of value option * (char * t) list

  let to_charlist s =
    let len = String.length s in
    let rec helper i acc =
      if i < 0 then acc
      else helper (i-1) (s.[i]::acc)
    in
    helper (len-1) []

  let empty = Node (None, [])


  let rec find_helper (trie:t) (ks:char list) : t =
    match (trie, ks) with
    | n, [] -> n
    | Node (_, children), h::t ->
      try
        find_helper (snd (List.find (fun child -> fst child = h) children)) t
      with
      | Not_found -> empty

  let find (trie:t) (s:string) : value option =
    match find_helper trie (to_charlist s) with
    | Node (None, _) -> None
    | Node (v,_) -> v


  let member (trie:t) (s:string) : bool =
    match find trie s with
    | None -> false
    | _ -> true

  let rec insert_helper (trie:t) (ks:char list) (v:value) : t =
    match (trie, ks) with
    | Node (_,children), [] -> Node (Some v, children)
    | Node (value, children), h::t ->
      let t' = try
        snd (List.find (fun child -> fst child = h) children)
      with
      | Not_found -> empty in
      let t'' = insert_helper t' t v in
      let new_children = List.rev_map (fun c -> if fst c = h then (h, t'') else c) children in
      Node (value, new_children)

  let insert (trie:t) (s:string) (v:value) : t =
    insert_helper trie (to_charlist s) v

  let rec get_all_nodes (trie:t) (f:value -> bool) (base:string) (acc:string list):string list =
    let fold_f acc' i =
      get_all_nodes (snd i) f (base^Char.escaped (fst i)) acc' in
    match trie with
    | Node(None, []) -> acc
    | Node(Some v, []) -> if f v then base::acc else acc
    | Node(None, children) ->
      List.fold_left fold_f acc children
    | Node(Some v, children) ->
      if f v then List.fold_left fold_f (base::acc) children
      else
        List.fold_left fold_f acc children


  let begin_with (trie:t) (f:value -> bool) (s:string) : string list =
    let found = find_helper trie (to_charlist s) in
    get_all_nodes found f s []

end
