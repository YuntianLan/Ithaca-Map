open Yojson.Basic;;
open Yojson.Basic.Util;;


module type MapGraph = sig

	type node
	type t

	val init_graph : string -> t
	val get_node_by_coord : float -> float -> t -> node
	val get_node_by_name : string -> t -> node
	val shortest_path : node -> node -> t -> node list
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
 * during the server-client interaction, so the average 
 * access time is logarithmic
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

	type node = nd

	type t = {
		(* Given coordinate, find the closest of ALL nodes *)
		loc_tree: kdtree;
		(* Given coordinate, find the closest of WAY nodes *)
		route_tree: kdtree;
		(* Given a place's name, find the "place" of that name *)
		name_trie: trie;

		(* 	Given a node id (int), find all the nid of
	   * the nodes reachable by walk/drive *)
		walk_table: (int list) H.t;
		drive_table: (int list) H.t;

		(* Given a node id (int), get the actual node *)
		node_table: (nd) H.t;
		(* Given a way id (int), get the actual way *)
		way_table: (way) H.t;
	}


	let num_nodes = 93987
	let num_ways = 14101


	let j2node n = 
		let id = n |> member "id" |> to_string |> int_of_string in 
		let lat = n |> member "lat" |> to_string |> float_of_string in
		let lon = n |> member "lon" |> to_string |> float_of_string in
		let tags = n |> member "tags" |> to_assoc |> 
			List.map (fun (x,y) -> x, to_string y) in
		let name = 
		(match List.filter (fun (x,_) -> x == "name") tags with
			| [] -> "" 
			| h::_ -> (snd h)
		) in
		let category = None in
		{nid = id; lat = lat; lon = lon; catego = category;
		name = name; tags = tags}


	let j2way n = 
		let nodes = n |> member "nodes" |> to_list |>
				List.map to_string |> List.map int_of_string in
		let id = n |> member "id" |> to_string |> int_of_string in
		let tags = n |> member "tags" |> to_assoc |>
				List.map (fun (x,y) -> (x,to_string y)) |>
				List.filter (fun (x,_) -> 
					((String.length x)<4) ||
					let sb = String.sub x 0 4
					in (not (sb="tige" || sb="is_i" || sb="sour"))) in
		let allow = match List.assoc_opt "highway" tags with
			| None -> Neither
			| Some name -> 
				let name = String.lowercase_ascii name in
				if name = "motorway" then Drive
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


	let build_edge_table (ways: way list) = 
		let table = H.create num_nodes in
		let add_edge_oneway n1 n2 =
			let n1_in = H.mem table n1 in
			let _ =
				if n1_in then H.replace table n1 (n2::(H.find table n1))
				else H.add table n1 [n2] in
			()
		in
		let add_edge n1 n2 =
			let n1_in = H.mem table n1 in
			let n2_in = H.mem table n2 in
			let _ =
				if n1_in then H.replace table n1 (n2::(H.find table n1))
				else H.add table n1 [n2] in
			let _ =
				if n2_in then H.replace table n2 (n1::(H.find table n2))
				else H.add table n2 [n1] in
			()
		in
		let rec add_edges lst = 
			match lst with
			| h1::h2::t ->
				let _ = add_edge h1 h2 in
				add_edges (h2::t)
			| _ -> () in
		let rec add_edges_oneway lst =
			match lst with
			| h1::h2::t ->
				let _ = add_edge_oneway h1 h2 in
				add_edges_oneway (h2::t)
			| _ -> () in
		let is_oneway w = 
			match List.assoc_opt "oneway" (w.tags) with
			| None -> false
			| Some s -> s = "yes" in
		let process_way w = 
			if is_oneway w then add_edges_oneway w.nodes
			else add_edges w.nodes in
		let _ = List.map process_way ways in
		let uniq k v = Some (List.sort_uniq (fun a b -> a - b) v) in
		let _ = H.filter_map_inplace uniq table in
		table


	let init_graph s = 
		let j = from_file s in
		let node_lst = j |> member "nodes" |> to_list |> List.map j2node in
		let way_lst = j |> member "ways" |> to_list |> List.map j2way in

		let walk_ways = List.filter 
			(fun w -> w.allow = Walk || w.allow = Both) way_lst in
		let drive_ways = List.filter 
			(fun w -> w.allow = Drive || w.allow = Both) way_lst in

		failwith "Unimplemented"



	let node_to_coord n = (n.lat, n.lon)


	let get_node_by_coord lat lon map =
		failwith "Unimplemented"

	let get_node_by_name name map =
		failwith "Unimplemented"



	(* Given the nid of two nodes, return the (approximate)
	 * distance between them in kilometers *)
	let distance n1 n2 nd_table = 
		let nd1 = H.find nd_table n1 in
		let nd2 = H.find nd_table n2 in
		let lat1, lon1 = node_to_coord nd1 in
		let lat2, lon2 = node_to_coord nd2 in
		let r_lat = 111.19492665183317 in
		let r_lon = 82.03674088387993 in
		let diff_lat, diff_lon = 
			r_lat *.(lat1 -. lat2), r_lon *. (lon1 -. lon2) in
		sqrt ((diff_lat *. diff_lat) +. (diff_lon *. diff_lon))

(* 	let distance n1 n2 = 
		let lat1, lon1 = n1.lat, n1.lon in
		let lat2, lon2 = n2.lat, n2.lon in
		let r_lat = 111.19492665183317 in
		let r_lon = 82.03674088387993 in
		let diff_lat, diff_lon = 
			r_lat *.(lat1 -. lat2), r_lon *. (lon1 -. lon2) in
		sqrt ((diff_lat *. diff_lat) +. (diff_lon *. diff_lon)) *)

	(* End condition for A*, returns true when two nodes
	 * are less than 20 meters apart *)
	let close_enough n1 n2 nd_table = 
		(distance n1 n2 nd_table) < 0.02

	(* Scoring function for A*, uses current path length
	 * as the current cost and l2 distance as heuristics *)
	let estimate (id,curr_dist,_) target nd_table =
		curr_dist +. (distance id target nd_table)

	(* Find the triple in the list and replace its value with
	 * the shortest distance, fail when the id not in the list *)
	let replace lst (id,dist,path)= 
		let rec replace_help lst (id,dist,path) acc =
			match lst with
			| [] -> failwith "replace key not found"
			| (id1,dist1,path1)::t ->
				if id1 = id then
					if dist<dist1 then (* dist is better, replace path1 with path *)
						t@((id,dist,path)::acc)
					else
						t@((id1,dist1,path1)::acc)
				else (* not this one, recurse *)
					replace_help t (id,dist,path) ((id1,dist1,path1)::acc)
		in
		replace_help lst (id,dist,path) []

	(* Merge two (nid * dist * path) lists, if duplicate id found
   * keep only the triple with lower distance *)
	let rec merge frontier expanded = 
		match expanded with
		| [] -> frontier
		| (id,dist,path)::t ->
			let matches (id1,_,_) = id1 = id in
			match List.find_opt matches frontier with
			| None -> merge ((id,dist,path)::frontier) t
			| Some (_,dist2,path2) ->
				let new_front = replace frontier (id,dist,path) in
				merge new_front t

	(* Given a (nid * dist * path) list, find the triple with the least
	 * expected distance to the destination, remove it from the list
	 * and return the result in the form of (triple * triple list) *)
	let find_best lst dest nd_table =
			let rec find_min lst curr_id curr_min = 
				(match lst with
				| [] -> curr_id
				| (id,dist,path)::t ->
					let new_est = estimate (id,dist,path) dest nd_table in
					if new_est < curr_min then
						find_min t id new_est
					else
						find_min t curr_id curr_min
			) in
			let best_id = find_min lst 0 999999. in
			let filt (id,_,_) = (not (id = best_id)) in
			let matches (id,_,_) = (id = best_id) in
			let triple = List.find matches lst in
			let remaining = List.filter filt lst in
			(triple, remaining)

	(* Expand a node (nid * dist * path list) triple into a list of triples *)
	let expand (id,dist,path) nd_table eg_table =
		let neighbor_ids = H.find eg_table id in
		let nodes = List.map (H.find nd_table) neighbor_ids in
		let exp_func n =
			let new_dist = dist +. distance id n.nid nd_table in
			let new_path = (n.nid)::path in
			(n.nid, new_dist, new_path) in
		List.map exp_func nodes

	let print_nd nd_table ndid =
		let n = H.find nd_table ndid in
		let lat = n.lat in let lon = n.lon in
		let _ = print_float lat in
		let _ = print_string ", " in
		let _ = print_float lon in
		let _ = print_char '\n' in ()


	let get_id (id,_,_) = id

 (* Takes the nid of the start and end nodes,
  * performs A* algorithm to find the shortest path between 2 nodes
  * and return as a list of nid, head is destination.
  * Assumes the start and end nid are in the way node table. *)
	let path_btw_nodes s e nd_table eg_table =
		let start = [(s,0.,[s])] in
		let explored = H.create num_nodes in
		let filt (id,dist,path) = close_enough id e nd_table in
		let rec exp_till_converge lst =
			let filtered = List.filter filt lst in
			if List.length filtered > 0 then filtered
			else
				let to_expand, remain = find_best lst e nd_table in
				let _ = (fun (id,_,_) -> H.add explored id true) to_expand in
				(* let estimate_remain = estimate to_expand e nd_table in *)

				(* let _ = print_endline (string_of_float estimate_remain) in *)
				let _ = print_endline (string_of_int (get_id to_expand)) in
				let _ = print_nd (get_id to_expand) in
				let _ = print_endline (string_of_int (List.length lst)) in
				let _ = print_endline (string_of_int (H.length explored)) in

				let expanded = expand to_expand nd_table eg_table in
				let filt_expanded = List.filter
					(fun (id,_,_) -> not (H.mem explored id)) expanded in
				let merged = merge remain filt_expanded in
				exp_till_converge merged
		in
		let satisfied = exp_till_converge start in
		let comp (_,d1,_) (_,d2,_) = 
			let diff = d1 -. d2 in
			if diff < 0. then (-1) else if diff > 0. then 1 else 0 in
		List.hd (List.sort comp satisfied)


	let shortest_path s e map = 

		failwith "Unimplemented"



end





















