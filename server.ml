(* open Lwt
open Cohttp
open Cohttp_lwt_unix *)
open Image
open Graph



module ImageTree = Image.Images
module MapGraph = Graph.Map

type tree = ImageTree.t

type graph = MapGraph.t
type node = MapGraph.node

type t = {
	imagetree: tree;
	mapgraph: graph;
}

let sth = Unix.socket
let cret = Thread.create

let sip = Unix.inet_addr_of_string "127.0.0.1"
let sport = 4780


let init_server = 
	let gr = MapGraph.init_graph "graph/full.json" in
	let tr = ImageTree.init () in
	{imagetree = tr; mapgraph = gr}


let make_buffer n =
	let rec make_help n acc = 
		if n = 0 then acc
		else make_help (n-1) (" "^acc) in
	Bytes.of_string (make_help n "")


let rec handle_client tg desc = 
	let str = make_buffer 1024 in
	let len = Unix.recv desc str 0 1024 [] in
	let sstr = Bytes.to_string str in
	let _ = print_endline sstr in
	let _ = Unix.send_substring desc sstr 0 10 [] in
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
	handle_client tg desc


let _ = begin_service init_server









