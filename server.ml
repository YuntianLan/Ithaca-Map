(* open Lwt
open Cohttp
open Cohttp_lwt_unix *)
open Image
open Graph
open Camlimages
open Pervasives



module ImageTree = Image.Images
module MapGraph = Graph.Map

type tree = ImageTree.t

type graph = MapGraph.t
type node = MapGraph.node

type t = {
	imagetree: tree;
	mapgraph: graph;
}


let sip = Unix.inet_addr_of_string "127.0.0.1"
let sport = 4999


let init_server =
	let gr = MapGraph.init_graph "graph/full.json" in
	let tr = ImageTree.init () in
	{imagetree = tr; mapgraph = gr}



let make_buffer n =
	let rec make_help n acc =
		if n = 0 then acc
		else make_help (n-1) (" "^acc) in
	Bytes.of_string (make_help n "")




(* [image_to_string name] returns the image specified by the
 * argument name in the form of a string where each
 * character represents a uint8 *)
let image_to_string name =
	let img = Png.load_as_rgb24 name [] in
	let s = Bytes.make (256*256*3) ' ' in
	let im = match img with
	| Rgb24 m -> m
	| _ -> failwith "won't happen" in
	for w = 0 to 255 do
		for h = 0 to 255 do
			let color = Rgb24.get im w h in
			let r, g, b = color.r, color.g, color.b in
			let br = char_of_int r in
			let bg = char_of_int g in
			let bb = char_of_int b in
			Bytes.set s (w * 256 + h + 0) br;
			Bytes.set s (w * 256 + h + 1) bg;
			Bytes.set s (w * 256 + h + 2) bb;
		done;
	done;
	s




let rec handle_client tg desc =
	let str = make_buffer 1024 in
	let len = Unix.recv desc str 0 1024 [] in
	let _ = print_endline "received message from client" in
	let s = Bytes.to_string (Bytes.sub str 0 len) in
	let args = String.split_on_char ' ' s in
	let idx = List.hd args in
	if idx = "1" then
		let res =
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
				slat ^ " " ^ slon in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
	else if idx = "2" then
		let res =
			if (String.length s) < 3 then
				"Error: string length for service 2 too short"
			else
				let name = String.sub s 2 ((String.length s)-2) in
				match MapGraph.get_node_by_name name tg.mapgraph with
				| None -> "Error: location not found"
				| Some nd ->
					let lat, lon = MapGraph.node_to_coord nd in
					(string_of_float lat) ^ " " ^ (string_of_float lon) in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
	else if idx = "3" then
		let res = "Error: idx 3 not implemented" in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
	else if idx = "4" then
		let res = image_to_string "tiles/0.png" in
		let _ = Unix.send desc res 0 (256*256*3) [] in
		handle_client tg desc
	else if ((idx = "quit") || (idx = "exit")) then
		let _ = Unix.close desc in
		(Thread.exit ())
	else
		let res = "Error: improper request" in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc



let rec test s () =
	let _ = print_endline s in
	let _ = Thread.delay 2. in
	let _ = Thread.yield () in
	test s ()

let begin_service tg =
	let server_socket =
		Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let _ = Unix.bind server_socket
		(Unix.ADDR_INET(sip, sport)) in
	let _ = Unix.listen server_socket 2 in
	let desc, addr = Unix.accept server_socket in
	let _ = print_endline "connected" in
	let thr = Thread.create (handle_client tg) desc in
	Thread.join thr;
	handle_client tg desc


let _ = begin_service init_server
