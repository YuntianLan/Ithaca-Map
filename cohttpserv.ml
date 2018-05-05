open Lwt
open Cohttp
open Cohttp_lwt_unix
open Image
open Graph
open Camlimages
open Pervasives



module ImageTree = Image.Images
module MapGraph = Graph.Map

type param = ImageTree.params

type tree = ImageTree.t

type graph = MapGraph.t
type node = MapGraph.node

type t = {
	imagetree: tree;
	mapgraph: graph;
}


let sip = Unix.inet_addr_of_string "127.0.0.1"
let sport = 5001

let init_server () =
	let gr = MapGraph.init_graph "graph/full.json" in
	let tr = ImageTree.init () in
	{imagetree = tr; mapgraph = gr}



(* [responst_bytes] tg s takes in the tree/graph and
 * the request string, respond with a stream of bytes *)
let response_str tg s =
	let args = String.split_on_char ' ' s in
	let idx = List.hd args in
	if idx = "1" then
		let res = try
				if (List.length args) < 3 then
						"Error: too few arguments for service 1"
				else
					let lat = float_of_string (List.nth args 1) in
					let lon = float_of_string (List.nth args 2) in
					let nd =
						MapGraph.get_node_by_coord lat lon tg.mapgraph in
					let rlat, rlon = MapGraph.node_to_coord nd in
					let slat = string_of_float rlat in
					let slon = string_of_float rlon in
						slat ^ " " ^ slon
			with _ ->
				"Error: invalid command for service 1"
		in res
		(* Location name to node coord *)
	else if idx = "2" then
		let res = try
				if (String.length s) < 3 then
					"Error: string length for service 2 too short"
				else
					let name = String.sub s 2 ((String.length s)-2) in
					match MapGraph.get_node_by_name name tg.mapgraph with
					| None -> "Error: location not found"
					| Some nd ->
						let lat, lon = MapGraph.node_to_coord nd in
							(string_of_float lat) ^ " " ^ (string_of_float lon)
			with _ ->
				"Error: invalid command for service 2"
		in res
		(* Path from one coord to another coord *)
	else if idx = "3" then
		try
			let drive = (List.nth args 1) = "drive" in
			let slat = float_of_string (List.nth args 2) in
			let slon = float_of_string (List.nth args 3) in
			let elat = float_of_string (List.nth args 4) in
			let elon = float_of_string (List.nth args 5) in
			let ns = MapGraph.get_node_by_coord slat slon tg.mapgraph in
			let ne = MapGraph.get_node_by_coord elat elon tg.mapgraph in
			let trip_len, path = MapGraph.find_path drive
					ns ne tg.mapgraph in
			let floats = List.map MapGraph.node_to_coord path in
			let strs = List.map (fun (la,lo) ->
					(string_of_float la) ^ "," ^ (string_of_float lo)) floats in
			let accum a b = a ^ ";" ^ b in
			let total_str = List.fold_left accum (List.hd strs) (List.tl strs) in
			let res_bts = (string_of_float trip_len) ^ " " ^ total_str in
				res_bts
		with
			| _ -> "illegal arguments for service 3"
		(* Result and image given client param *)
	else if idx = "4" then
		let _ = print_endline "entered 4" in
		let prm : param option = try Some({
				param_upleft_lon = float_of_string (List.nth args 2);
				param_upleft_lat = float_of_string (List.nth args 1);
				param_lowright_lon = float_of_string (List.nth args 4);
				param_lowright_lat = float_of_string (List.nth args 3);
				width = float_of_string (List.nth args 5);
				height = float_of_string (List.nth args 6);
			})
			with _ -> None in
		let _ = print_endline "build param" in
		begin match prm with
			| None ->
				let _ = print_endline "nothing, sending" in
					"Error: exception in parsing parameter!"
			| Some pm ->
				let _ = print_endline "something, sending" in
				let rt = ImageTree.query_image tg.imagetree pm in
				let fullmap_info = ImageTree.build_full_map rt in
				let bts = fst fullmap_info in
				let fullw = fullmap_info |> snd |> fst |> string_of_int in
				let fullh = fullmap_info |> snd |> fst |> string_of_int in
				let lol1 = string_of_float rt.res_upleft_lon in
				let lal1 = string_of_float rt.res_upleft_lat in
				let lor1 = string_of_float rt.res_lowright_lon in
				let lar1 = string_of_float rt.res_lowright_lat in
				let depth = string_of_int rt.tree_depth in
				let status = if rt.status then "1" else "0" in
				let res1 = fullw ^ " " ^ fullh ^ " " ^ lal1 ^ " " ^ lol1 ^ " " ^
									 lar1 ^ " " ^ lor1 ^ " " ^ depth ^ " " ^ status ^ "$" in
				res1
		end
		(* Error *)
	else
		"Error: improper request"












let server =
	let callback _conn req body =
		let uri = req |> Request.uri |> Uri.to_string in
		let meth = req |> Request.meth |> Code.string_of_method in
		let headers = req |> Request.headers |> Header.to_string in
		let _ = print_endline (uri) in
		body |> Cohttp_lwt.Body.to_string >|= (fun body ->
				(Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
					uri meth headers body))
		>>= (fun body -> Server.respond_string 
			~headers:(Header.init_with "Access-Control-Allow-Origin" "*")
			~status:`OK ~body:"hello world" ())
		(* (body |> Cohttp_lwt.Body.to_string) >>= 
			 (fun body -> Server.respond_string ~status:`OK ~body:"hello world" ()) *)
	in
	Server.create ~mode:(`TCP (`Port 8001)) (Server.make ~callback ())

let server2 =
	let callback _conn req body =
		let uri = req |> Request.uri |> Uri.to_string in
		let meth = req |> Request.meth |> Code.string_of_method in
		let headers = req |> Request.headers |> Header.to_string in
		let _ = print_endline (uri) in
		body |> Cohttp_lwt.Body.to_string >|= (fun body ->
				(Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
					uri meth headers body))
		>>= (fun body -> Server.respond_file
			~headers:(Header.init_with "Access-Control-Allow-Origin" "*")
			~fname: "testcheap.png" ())
		(* (body |> Cohttp_lwt.Body.to_string) >>= 
			 (fun body -> Server.respond_string ~status:`OK ~body:"hello world" ()) *)
	in
	Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())



let () = ignore (Lwt_main.run server2)


















