open Yojson.Basic;;
open Yojson.Basic.Util;;


module type MapGraph = sig

	type node
	type t

	val init_graph : string -> t
	val get_node_by_coord : float -> float -> t -> node
	val get_node_by_name : string -> t -> node option
	val find_path : bool -> node -> node -> t -> float * node list
	val node_to_coord : node -> (float * float)

end


type category = 
| FoodDrink | Shop | Medical | Study | Fuel | Other


type allowed = Walk | Drive | Both | Neither

(* Node type, represents a point location on the map
 * with id, latitude and longitude stored *)
type nd = {
	nid: int;
	lat: float;
	lon: float;

	catego: category;
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
| LatNode of float * kdtree * kdtree
| LonNode of float * kdtree * kdtree
| Leaf of int


(* Slice the list into two parts starting from start,
 * Example: sublists [1;2;3;4;5] 2 = ([1;2],[3;4;5]) 
 * Precondition: the start position is valid *)
let sublists lst start =
	let _ = assert (start<(List.length lst)) in
	let rec sublist_help lst start acc = 
		if start = 0 then (List.rev acc), lst
		else
			sublist_help (List.tl lst) 
				(start - 1) ((List.hd lst)::acc)
	in
	sublist_help lst start []


(* The procedure to build a kdtree, each non-leaf node
 * corresponds to a split, all nodes greater than or
 * equal to go the the right and the left go to left*)
let rec build_kdtree nodes is_lat =
	if (List.length nodes) = 0 then
		failwith "build_kdtree encounters empty list"
	else if (List.length nodes) = 1 then
		Leaf((List.hd nodes).nid)
	else
		if is_lat then
			(* sort accodring to lat, divide *)
			let comp n1 n2 = let diff = n1.lat -. n2.lat in
				if diff < 0. then -1
				else if diff = 0. then 0
				else 1
			in
			let sorted = List.sort comp nodes in
			let idx = (List.length sorted) / 2 in
			let split_val = (List.nth sorted idx).lat in
			let left, right = sublists sorted idx in
			LatNode (split_val,
						build_kdtree left false,
						build_kdtree right false)
		else
			let comp n1 n2 = let diff = n1.lon -. n2.lon in
				if diff < 0. then -1
				else if diff = 0. then 0
				else 1
			in
			let sorted = List.sort comp nodes in
			let idx = (List.length sorted) / 2 in
			let split_val = (List.nth sorted idx).lon in
			let left, right = sublists sorted idx in
			LonNode (split_val,
						build_kdtree left true,
						build_kdtree right true)




type place = Nodeid of int | Wayid of int


module Map : MapGraph = struct


	module H = IntHashtbl

	type place_trie = (place list) Trie.trie

	type node = nd

	type t = {
(* 		(* Given coordinate, find the closest of ALL nodes *)
		loc_tree: kdtree;
		(* Given coordinate, find the closest of WAY nodes *)
		route_tree: kdtree;
 *)

		(* At this moment, use id list to store all nodes and
		 * nodes on the way *)
		all_nodes: int list;
		walkway_nodes: int list;
		driveway_nodes: int list;

		(* Given a place's name, find the "place" of that name *)
		name_trie: place_trie;

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

	(* Convert a json to a node object *)
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
		let categ = match (List.assoc_opt "amenity" tags, 
			List.assoc_opt "shop" tags) with
			| None, None -> Other
			| Some name1, None ->
				let name = String.lowercase_ascii name1 in
				if name = "fast_food" then FoodDrink
				else if name = "restaurant" then FoodDrink
				else if name = "cafe" then FoodDrink
				else if name = "pub" then FoodDrink
				else if name = "bar" then FoodDrink
				else if name = "ice_cream" then FoodDrink
				else if name = "school" then Study
				else if name = "university" then Study
				else if name = "college" then Study
				else if name = "library" then Study
				else if name = "fuel" then Fuel
				else if name = "doctors" then Medical
				else if name = "hospital" then Medical
				else if name = "pharmacy" then Medical
				else Other
			| None, Some name2 -> Shop
			| Some name1, Some name2 -> Shop in
		{nid = id; lat = lat; lon = lon; catego = categ;
		name = name; tags = tags}

	(* Convert a json to a way object *)
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
		let categ = match (List.assoc_opt "amenity" tags, 
			List.assoc_opt "shop" tags) with
			| None, None -> Other
			| Some name1, None ->
				let name = String.lowercase_ascii name1 in
				if name = "fast_food" then FoodDrink
				else if name = "restaurant" then FoodDrink
				else if name = "cafe" then FoodDrink
				else if name = "pub" then FoodDrink
				else if name = "bar" then FoodDrink
				else if name = "ice_cream" then FoodDrink
				else if name = "school" then Study
				else if name = "university" then Study
				else if name = "college" then Study
				else if name = "library" then Study
				else if name = "fuel" then Fuel
				else if name = "doctors" then Medical
				else if name = "hospital" then Medical
				else if name = "pharmacy" then Medical
				else Other
			| None, Some name2 -> Shop
			| Some name1, Some name2 -> Shop in
		{wid = id; nodes = nodes; categ = categ;
			name = name; allow = allow; tags = tags}

	let nodes_oftype leftuplat leftuplon rightdownlat rightdownlon type graph =
		let 

	let ways_oftype leftuplat leftuplon rightdownlat rightdownlon type graph =
		failwith "unimplmented"

	let nodes_ways_oftype leftuplat leftuplon rightdownlat rightdownlon type graph =
		(nodes_oftype lat lon type) @ (ways_oftype lat lon type) 

	(* Given a node list, build a hashtable mapping from
	 * node ids to nodes *)
	let build_node_table nodes = 
		let table = H.create num_nodes in
		let add_node n = 
			let key = n.nid in
			H.replace table key n in
		let _ = List.map add_node nodes in
		table

	(* Given a way list, build a hashtable mapping from
	 * way ids to ways *)
	let build_way_table ways = 
		let table = H.create num_ways in
		let add_way w = 
			let key = w.wid in
			H.replace table key w in
		let _ = List.map add_way ways in
		table

	(* Given a way list, build the adjacency matrix that
	 * mappes node id to a list of node ids of its neighbors *)
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





	(* Add a given node's nid to the trie *)
	let add_node_trie (tr:place_trie) (nd:node) = 
		if nd.name = "" then tr
		else
			if Trie.memb tr nd.name then
				let lst = match Trie.find tr nd.name with
				| None -> failwith "this won't happen"
				| Some l -> l in
				Trie.insert tr nd.name (Nodeid(nd.nid)::lst)
			else Trie.insert tr nd.name [Nodeid(nd.nid)]

	(* Add a given way's wid to the trie *)
	let add_way_trie (tr:place_trie) (wy:way) = 
		if wy.name = "" then tr
		else
			if Trie.memb tr wy.name then
				let lst = match Trie.find tr wy.name with
				| None -> failwith "this won't happen"
				| Some l -> l in
				Trie.insert tr wy.name (Wayid(wy.wid)::lst)
			else Trie.insert tr wy.name [Wayid(wy.wid)]


(*

	let add_node_trie (tr:place_trie) (nd:node) = 
		if nd.name = "" then tr
		else
			if memb tr nd.name then
				let lst = match find tr nd.name with
				| None -> failwith "this won't happen"
				| Some l -> l in
				insert tr nd.name (Nodeid(nd.nid)::lst)
			else insert tr nd.name [Nodeid(nd.nid)]

	let add_way_trie (tr:place_trie) (wy:way) = 
		if wy.name = "" then tr
		else
			if memb tr wy.name then
				let lst = match find tr wy.name with
				| None -> failwith "this won't happen"
				| Some l -> l in
				insert tr wy.name (Wayid(wy.wid)::lst)
			else insert tr wy.name [Wayid(wy.wid)]
*)






	let way2node ways = 
		let nodes = List.map (fun w -> w.nodes) ways in
		let lst = List.flatten nodes in
		let comp n1 n2 = n1 - n2 in
		List.sort_uniq comp lst




	let init_graph file_name = 
		let j = from_file file_name in
		let node_lst = j |> member "nodes" |> to_list |> List.map j2node in
		let way_lst = j |> member "ways" |> to_list |> List.map j2way in

		let all_nodes = List.map (fun x -> x.nid) node_lst in

		let walk_ways = List.filter 
			(fun w -> w.allow = Walk || w.allow = Both) way_lst in
		let drive_ways = List.filter 
			(fun w -> w.allow = Drive || w.allow = Both) way_lst in

		let walkway_nodes = way2node walk_ways in
		let driveway_nodes = way2node drive_ways in

		let tr = Trie.empty in
		let tr = List.fold_left add_node_trie tr node_lst in
		let tr = List.fold_left add_way_trie tr way_lst in

		let nd_table = build_node_table node_lst in
		let way_table = build_way_table way_lst in

		let walk_table = build_edge_table walk_ways in
		let drive_table = build_edge_table drive_ways in {
			all_nodes = all_nodes;
 			walkway_nodes = walkway_nodes;
 			driveway_nodes = driveway_nodes;
			name_trie = tr;
			walk_table = walk_table;
			drive_table = drive_table;
			node_table = nd_table;
			way_table = way_table;
		}


	let node_to_coord n = (n.lat, n.lon)


	let coord_dist lat1 lon1 lat2 lon2 = 
		let r_lat = 111.19492665183317 in
		let r_lon = 82.03674088387993 in
		let diff_lat, diff_lon = 
			r_lat *.(lat1 -. lat2), r_lon *. (lon1 -. lon2) in
		sqrt ((diff_lat *. diff_lat) +. (diff_lon *. diff_lon))


	(* Given the nid of two nodes, return the (approximate)
	 * distance between them in kilometers *)
	let distance n1 n2 nd_table = 
		let nd1 = H.find nd_table n1 in
		let nd2 = H.find nd_table n2 in
		let lat1, lon1 = node_to_coord nd1 in
		let lat2, lon2 = node_to_coord nd2 in
		coord_dist lat1 lon1 lat2 lon2

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
					if dist<dist1 then 
					(* dist is better, replace path1 with path *)
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
				(* let _ = print_nd (get_id to_expand) in *)
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

	(* Helper function to find the closest node id given coordinates,
	 * lst is int list for node id, (lat,lon) is the target
	 * coordinate, nd_table is the hashtable storing information,
	 * curr_id and curr_min are the id and distance of current closest *)
	let rec find_closest lst (lat,lon) nd_table curr_id curr_min =
		match lst with
		| [] -> curr_id
		| h::t ->
			let h_node = H.find nd_table h in
			let h_lat, h_lon = h_node.lat, h_node.lon in
			let d = coord_dist lat lon h_lat h_lon in
			if d < curr_min then
				find_closest t (lat,lon) nd_table h d
			else
				find_closest t (lat,lon) nd_table curr_id curr_min


	let get_node_by_coord lat lon map =
		let nid = find_closest map.all_nodes (lat,lon) 
			map.node_table 0 99999.9 in
		H.find map.node_table nid

	(* TODO: improve when multiple nodes possible? *)
	let get_node_by_name name map =
		match Trie.find map.name_trie name with
		| None -> None
		| Some p ->
			(* If multiple entries, select the first one *)
			match (List.hd p) with
			| Nodeid nid -> Some (H.find map.node_table nid)
			| Wayid wid -> 
				let w = H.find map.way_table wid in
				let nid = List.hd w.nodes in
				Some (H.find map.node_table nid)

	let get_snd (_,e,_) = e
	let get_trd (_,_,e) = e


	let find_path (drive:bool) (s:nd) (e:nd) (map:t) = 
		let tbl, ndlst =
			if drive then map.drive_table, map.driveway_nodes
			else map.walk_table, map.walkway_nodes in
		let slat, slon = s.lat, s.lon in
		let elat, elon = e.lat, e.lon in
		let sid = find_closest 
			ndlst (slat,slon) map.node_table 0 99999.9 in
		let eid = find_closest
			ndlst (elat,elon) map.node_table 0 99999.9 in
		let (dist:float), (path:int list) = 
			let triple = (path_btw_nodes sid eid map.node_table tbl) in
			get_snd triple, get_trd triple in
		let (node_path:nd list) = List.map (H.find map.node_table) path in
		(dist, node_path)


	let path_by_names drive s e map = 
		let ns = match get_node_by_name s map with
			| None -> failwith "start name not found"
			| Some p -> p in
		let ne = match get_node_by_name e map with
			| None -> failwith "end name not found"
			| Some q -> q in
		find_path drive ns ne map

end





















