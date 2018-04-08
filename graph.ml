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
| FoodDrink
| School | Bank | Cinema | Fuel
| Postbox | Carwash | Doctor | Library
| Other | Road


type allowed = Walk | Drive | Both | Neither

(* Node type, represents a point location on the map
 * with id, latitude and longitude stored *)
type nd = {
	nid: int;
	lat: float;
	lon: float;

	catego: category option;
	name: string;
	(* TODO: add tags when building nodes in the future *)
	tags: (string * string) list;
}


type way = {
	wid: int;
	nodes: int list;
	categ: category option;
	name: string;
	allow: allowed;
	(* TODO: add tags when building nodes in the future *)
	tags: (string * string) list;
}

module IntHash = struct

	type t = int

	let equal n1 n2 = n1 = n2
	let hash n = n

end

module IntHashtbl = Hashtbl.Make(IntHash)


(* The kdtree structure is used here to quickly index the 
 * nearest neighbor node given a coordinate (float * float).
 * The tree is created at the parsing phase and never modified
 * during the server-client interaction, so the average access time 
 * is logarithmic
 *)
type kdtree = 
| Xnode of float * kdtree * kdtree
| Ynode of float * kdtree * kdtree
| Leaf of int


type place = Nodeid of int | Wayid of int


(* The prefix tree structure is used to quickly index all the
 * relevant nodes given a place's name (string), the tree is 
 * created at the parsing phase and never modified after.
 * TODO: Put TRIE to the outside in a seperate module
 *)
type trie = {
	acc: string;
	curr_char: string;
	nodes: place list;
	children: trie list;
}





module Map : MapGraph = struct
	
	module H = IntHashtbl

	(* Given coordinate, find the closest of ALL nodes *)
	type loc_tree = kdtree
	(* Given coordinate, find the closest of WAY nodes *)
	type route_tree = kdtree

	(* Given a place's name, find the "place" of that name *)
	type name_trie = trie

	(* Given a node id (int), find all the nid of
	 * the nodes reachable by walk *)
	type edge_table = (int list) H.t

	(* Given a node id (int), get the actual node *)
	type node_table = (nd) H.t
	(* Given a way id (int), get the actual way *)
	type way_table = (way) H.t

	type node = nd

	type t = loc_tree * route_tree * name_trie * 
		edge_table * edge_table * node_table * way_table
	(* walk_table   drive_table *)


	let num_nodes = 93987
	let num_ways = 14101


	let j2node n = 
		let id = n |> member "id" |> to_string |> int_of_string in 
		let lat = n |> member "lat" |> to_string |> float_of_string in
		let lon = n |> member "lon" |> to_string |> float_of_string in 
		let tags = n |> member "tags" |> to_assoc |> List.map (fun (x,y) -> x, to_string y) in
		let name = (match List.filter (fun (x,_) -> x == "name") tags with
			| [] -> "" 
			| h::_ -> (snd h)
		) in
		let category = None in
		{nid = id; lat = lat; lon = lon; catego = category; name = name; tags = tags}


	let j2way n = 
		let nodes = n |> member "nodes" |> to_list |>
				List.map to_string |> List.map int_of_string in
		let id = n |> member "id" |> to_string |> int_of_string in
		let tags = n |> member "tags" |> to_assoc |>
				List.map (fun (x,y) -> (x,to_string y)) |>
				List.filter (fun (x,_) -> 
					((String.length x)<4) ||
					let sb = String.sub x 0 4
					in (not (sb="tige" || sb="is_i"))) in
		let allow = match List.assoc_opt "highway" tags with
			| None -> Neither
			| Some name -> 
				let name = String.lowercase_ascii name in
				if name = "mortorway" then Drive
				else if name = "primary" then Drive
				else if name = "secondary" then Both
				else if name = "tertiary" then Both
				else if name = "unclassified" then Both
				else if name = "residential" then Both
				else if name = "motorway_link" then Drive
				else if name = "secondary_link" then Both
				else if name = "tertiary_link" then Both
				else if name = "pedestrian" then Walk
				else if name = "track" then Drive
				else if name = "footway" then Walk
				else if name = "bridleway" then Walk
				else if name = "steps" then Walk
				else if name = "path" then Walk
				else if name = "cycleway" then Walk
			else if name = "service" then Both
				else Neither in 
		let name = match List.assoc_opt "name" tags with
		| None -> ""
		| Some s -> s in
		{wid = id; nodes = nodes; categ = None;
			name = name; allow = allow; tags = tags}


	let build_node_table nodes = 
		let table = H.create num_nodes in
		let add_node n = 
			let key = n.nid in
			H.replace table key n in
		let _ = List.map add_node nodes in
		table


	let build_way_table ways = 
		let table = H.create num_ways in
		let add_way w = 
			let key = w.wid in
			H.replace table key w in
		let _ = List.map add_way ways in
		table


	let build_edge_table ways = 
		let table = H.create num_nodes in
		let nid_llst = List.map (fun w -> w.nodes) ways in
		let add_edge n1 n2 = () in

		let rec add_edges lst = 
			match lst with
			| h1::h2::t ->
				failwith "Unimplemented"
			| _ -> () in ()


	let init_graph s = 
		let j = from_file s in
		let node_lst = j |> member "nodes" |> to_list |> List.map j2node in
		let way_lst = j |> member "ways" |> to_list |> List.map j2way in

		let walk_ways = List.filter 
			(fun w -> w.allow = Walk || w.allow = Both) way_lst in
		let drive_ways = List.filter 
			(fun w -> w.allow = Drive || w.allow = Both) way_lst in

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





















