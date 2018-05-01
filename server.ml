(*
                       _oo0oo_
                      o8888888o
                      88" . "88
                      (| -_- |)
                      0\  =  /0
                    ___/`---'\___
                  .' \\|     |// '.
                 / \\|||  :  |||// \
                / _||||| -:- |||||- \
               |   | \\\  -  /// |   |
               | \_|  ''\---/''  |_/ |
               \  .-\__  '-'  ___/-. /
             ___'. .'  /--.--\  `. .'___
          ."" '<  `.___\_<|>_/___.' >' "".
         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
         \  \ `_.   \_ __\ /__ _/   .-` /  /
     =====`-.____`.___ \_____/___.-`___.-'=====
                       `=---='


     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

               佛祖保佑         永无BUG
*)



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


let sip = Unix.inet_addr_of_string "10.132.12.66"
let sport = 4998


let init_server () =
	let gr = MapGraph.init_graph "graph/full.json" in
	let tr = ImageTree.init () in
	{imagetree = tr; mapgraph = gr}



let lst2str lst = 
	let rec helper l acc start = 
		match l with
		| [] -> acc ^ "]"
		| h::t -> 
			let nacc = if start then "[" ^ h
			else (acc ^ ";" ^ h) in
			helper t nacc false in
	helper lst "" true

let slst2str lst = 
	let rec helper l acc start = 
		match l with
		| [] -> acc ^ "]"
		| h::t -> 
			let nacc = if start then "[" ^ (lst2str h)
			else (acc ^ ";" ^ (lst2str h)) in
			helper t nacc false in
	helper lst "" true



let rec handle_client tg desc =
	let str = Bytes.make 1024 ' ' in
	let rec get_len () =
		let _ = print_endline "waiting for response" in
		if Thread.wait_timed_read desc 0.5 then
			Unix.recv desc str 0 1024 []
		else
			let _ = print_endline "yielding to other threads" in
			let _ = Thread.yield () in 
			get_len ()
	in
	let len = get_len () in
	let _ = print_endline "received message from client" in
	let s = Bytes.to_string (Bytes.sub str 0 len) in
	let args = String.split_on_char ' ' s in
	let idx = List.hd args in
	(* Coordinates to nearest node coord *)
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
		with _ -> "Error: invalid command for service 1"
		in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
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
		with _ -> "Error: invalid command for service 2"
		in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
	(* Path from one coord to another coord *)
	else if idx = "3" then
		let res = "Error: idx 3 not implemented" in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc
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
			let res = "Error: exception in parsing parameter!" in
			let _ = Unix.send_substring desc res 0
				(String.length res) [] in
			let _ = Unix.send desc (Bytes.make 10 'a') 0 10 [] in
			Thread.yield ();
			handle_client tg desc
		| Some pm ->
			let _ = print_endline "something, sending" in
			let rt = ImageTree.query_image tg.imagetree pm in
			let bts = ImageTree.build_full_map rt in
			let grid = slst2str rt.img_grid in
			let lol1 = string_of_float rt.res_upleft_lon in
			let lal1 = string_of_float rt.res_upleft_lat in
			let lor1 = string_of_float rt.res_lowright_lon in
			let lar1 = string_of_float rt.res_lowright_lat in
			let depth = string_of_int rt.tree_depth in
			let status = if rt.status then "1" else "0" in
			let res1 = grid ^ " " ^ lal1 ^ " " ^ lol1 ^ " " ^ 
				lar1 ^ " " ^ lor1 ^ " " ^ depth ^ " " ^ status in
			let _ = print_endline "sending res1" in
			Unix.send_substring desc res1 0 
				(String.length res1) [];
			let _ = print_endline "sending res2" in
			Unix.send desc bts 0 (Bytes.length bts) [];
			let _ = print_endline "finished, yielding" in
			Thread.yield ();
			handle_client tg desc
		end
	(* Path from one coord to another coord *)
	else if idx = "5" then
		let res = Bytes.make 12582912 ' ' in
		let _ = Unix.send desc res 0 12582912  [] in
		Thread.yield ();
		handle_client tg desc
	(* Client willingly terminates connection *)
	else if ((idx = "quit") || (idx = "exit")) then
		let _ = Unix.close desc in
		Thread.exit ()
	(* Error *)
	else
		let res = "Error: improper request" in
		let _ = Unix.send_substring desc res 0
			(String.length res) [] in
		Thread.yield ();
		handle_client tg desc



let rec test s () =
	print_endline s;
	Thread.delay 2.;
	Thread.yield ();
	test s ()

let rec begin_service tg =
	let server_socket =
		Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let _ = Unix.bind server_socket
		(Unix.ADDR_INET(sip, sport)) in
	let _ = Unix.listen server_socket 2 in
	let desc, addr = Unix.accept server_socket in
	let _ = print_endline "connected" in
	let thr = Thread.create (handle_client tg) desc in
	Thread.yield ();
	begin_service tg
	(* handle_client tg desc *)



let _ = begin_service (init_server ())
