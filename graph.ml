open Yojson.Basic;;
open Yojson.Basic.Util;;


module type MapGraph = sig

	type node
	type t

	val init_graph : string -> t
	val get_node_by_coord : float -> float -> t -> node
	val get_node_by_name : string -> t -> node
	val shortest_path : node -> node -> node list
	val node_to_coord : node -> (float * float)

end


type category = 
| Shop | Tourism | Leisure
| FoodDrink (* ^_^ *)
| School | Bank | Cinema | Fuel 
| Postbox | Carwash | Doctor | Library
| Other | Nope


(* Node type, represents a point location on the map
 * with id, latitude and longitude stored *)
type nd = {
	nid: int;
	lat: float;
	lon: float;

	categ: category;
	name: string;
	(* TODO: add tags when building nodes in the future *)
	tags: (string * string) list;
}



module NodeHash = struct

	type t = nd

	let equal n1 n2 = n1.nid = n2.nid
	let hash n = n.nid

end

module NodeHashtbl = Hashtbl.Make(NodeHash)


(* The kdtree structure is used here to quickly index the 
 * nearest neighbor node given a coordinate (float * float).
 * The tree is created at the parsing phase and never modified
 * during the server-client interaction, so the average access time 
 * is logarithmic
 *)
type kdtree = 
| Xnode of float * kdtree * kdtree
| Ynode of float * kdtree * kdtree
| Leaf of nd list


type place = Nodeid of int | Wayid of int

(* The prefix tree structure is used to quickly index all the
 * relevant nodes given a place's name (string), the tree is 
 * created at the parsing phase and never modified after.
 *)
type trie = {
	acc: string;
	curr_char: string;
	nodes: nd list;
	children: trie list;
}





module Map : MapGraph = struct
	
	module H = NodeHashtbl

	type tbl = (nd list) H.t

	type node = nd
	type t = kdtree * trie * tbl




	let j2node n = 
		let j = to_assoc n in
		let id = j |> List.assoc "@id" |> to_string |> int_of_string in
		let lat = j |> List.assoc "@lat" |> to_string |> float_of_string in
		let lon = j |> List.assoc "@lon" |> to_string |> float_of_string in
		let tags = List.assoc_opt "tag" j in
		match tags with
		| None -> {nid = id; lat = lat; lon = lon;
								categ = Nope; name = ""; tags = []}
		| Some tg -> failwith "Unimplemented"













	let j2way n = 
		let j = to_assoc n in ()
















	let init_graph s = 
		let j = from_file s in
		let l = j |> to_assoc |> List.hd |> snd |> to_assoc in
		let node_jlst = (List.nth l 5) |> snd |> to_list in
		let way_jlst = (List.nth l 6) |> snd |> to_list in
		failwith "Unimplemented"



	let get_node_by_coord lat lon map =
		failwith "Unimplemented"

	let get_node_by_name name map =
		failwith "Unimplemented"

	let shortest_path s e = 
		failwith "Unimplemented"

	let node_to_coord n = 
		failwith "Unimplemented"


end





















