(* Example for how the client interacts with the server,
 * current supported actions:
 * 1. Nearest node coord given coord
 * 2. Nearest node coord given location name
 * 3. Path from one coord to another coord
 *
 *
 *)


let cip = Unix.inet_addr_of_string "127.0.0.1"
let cport = 3110
let sport = 4780

(* Example string for service 1 *)
let eg_service1 = "1 42.813746 -76.7116266"

(* Example string for service 2 *)
let eg_service1 = "2 Texas Roadhouse"

(* Example string for service 3 *)
let eg_service1 = "3 42.813746 -76.7116266 42.6753 -76.11712"

let make_buffer n =
	let rec make_help n acc = 
		if n = 0 then acc
		else make_help (n-1) (" "^acc) in
	Bytes.of_string (make_help n "")


(* Setup the client connection socket *)
let init_client = 
	let client_socket = 
		Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let _ = Unix.bind client_socket 
		(Unix.ADDR_INET(cip, cport)) in
	let _ = Unix.connect 
		client_socket (Unix.ADDR_INET(cip,sport)) in
	client_socket

(* Send a string of information to the server *)
let send_info sock info = 
	Unix.send_substring sock info 0
		(String.length info) []

(* Receive a string of information to the server *)
let recv_info sock =
	let buffer = make_buffer 1024 in
	let num = Unix.recv sock buffer 0 1024 [] in
	String.sub (Bytes.to_string buffer) 0 num









