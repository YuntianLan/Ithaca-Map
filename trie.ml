(* reference: part of the code is inspired by https://www.lri.fr/~filliatr/ftp/ocaml/ds/trie.ml.html *)


(* AF: the Node(Some v, [(c1, Node(v1, [sub1])), (c2,Node(v2, [sub2])),...,(cn,Node(vn, [subn]))])
 * represents the Trie with root vaule v, and children with keys [c1...cn] associated to
 * [v1...vn] respectively. None represents that the node has no value or the key is not
 * end of a word
 * RI: There are no duplicate keys in the Trie *)
type 'a trie = Node of ('a option) * (char * 'a trie) list


let to_charlist s =
  let len = String.length s in
  let rec helper i acc =
    if i < 0 then acc
    else helper (i-1) (s.[i]::acc)
  in
  helper (len-1) []

let empty = Node (None, [])

(* [find_helper t ks] is the sub-trie that has key matched with [ks] in [t] *)
let rec find_helper (trie:'a trie) (ks:char list) : 'a trie =
  match (trie, ks) with
  | n, [] -> n
  | Node (_, children), h::t ->
    try
      find_helper (snd (List.find (fun child -> fst child = h) children)) t
    with
    | Not_found -> empty

let find (trie:'a trie) (s:string) : 'a option =
  match find_helper trie (s |> String.lowercase_ascii |> to_charlist) with
  | Node (None, _) -> None
  | Node (v,_) -> v


let memb (trie:'a trie) (s:string) : bool =
  match find trie s with
  | None -> false
  | _ -> true

(* [insert_helper t ks v] is the trie [t] with key value pair [ks],[v] inserted *)
let rec insert_helper (trie:'a trie) (ks:char list) (v:'a) : 'a trie =
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

let insert (trie:'a trie) (s:string) (v:'a) : 'a trie =
  insert_helper trie (s |> String.lowercase_ascii |> to_charlist) v

(* [get_all_nodes t f acc] is the list of all values that satisfies the predicate
 * [f] in [t] and [acc] *)
let rec get_all_nodes (trie:'a trie) (f:'a -> bool) (acc:'a list):'a list =
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


let begin_with (trie:'a trie) (f:'a -> bool) (s:string) : 'a list =
  let found = find_helper trie (s |> String.lowercase_ascii |> to_charlist) in
  get_all_nodes found f []
