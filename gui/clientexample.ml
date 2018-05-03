(* Example for how the client interacts with the server,
 * current supported actions:
 * 1. Nearest node coord given coord
 * 2. Nearest node coord given location name
 * 3. Path from one coord to another coord
 * 4. Result given a query parameter
 *
 *
 *
 *
 *)


let sip = Unix.inet_addr_of_string "127.0.0.1"
let cip = Unix.inet_addr_of_string "127.0.0.1"
let cport = 3418
let sport = 4996

(* Example string for service 1 *)
(* Order: lat; lon *)
let eg_service1 = "1 42.813746 -76.7116266"
let eg_resp1 = "42.813646 -76.7116234"

(* Example string for service 2 *)
let eg_service2 = "2 Texas Roadhouse"
let eg_resp2 = "42.813646 -76.7116234"

(* Example string for service 3 *)
let eg_service3 = "3 drive 42.813746 -76.7116266 42.6753 -76.11712"


(* Example string for service 4 *)
(* Order: upleft_lat, upleft_lon,
downright_lat, downright_lon, width, height *)
let eg_service4 = "4 43 -77 42 -76 1000 1000"
(* Transmitting images takes 2 steps and the server
 * will initiate 2 callbacks, the first for metadata
 * and the second for encoded image.
 *
 * The first response contains:
 * img_width; img_height;
 * downright_lat; downright_lon;
 * tree_depth; statue(1:true/0:false);
 * length of incoming byte encoded image
 * *)
let eg_resp4_1 =
	"512 512 42.0 -76.0 43.0 -75.0"^
	"3 1 1024"
let eg_resp4_2 = Bytes.make 1024 ' '


(* Setup the client connection socket *)
let init_client () =
	let client_socket =
		Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let () = print_endline  "sdf" in
	let _ = Unix.bind client_socket
		(Unix.ADDR_INET(cip, cport)) in
	let () = print_endline  "absa" in
	let _ = Unix.connect
		client_socket (Unix.ADDR_INET(sip,sport)) in
	client_socket


(* Send a string of information to the server *)
let send_info sock info : int =
	Unix.send_substring sock info 0 (String.length info) []


(* Examples for client receiving information from the
 * server, it's the client's responsibility to distinguish
 * which one to use. *)

(* Receive a string of information to the server *)
let recv_info sock =
	let buffer = Bytes.make 196608 ' ' in
	let num = Unix.recv sock buffer 0 196608 [] in
	String.sub (Bytes.to_string buffer) 0 num

(* [recv_bytes sock num] returns the bytes with a
 * maximum length of num. *)
let recv_bytes sock num =
	let buffer = Bytes.make num ' ' in
	let l = Unix.recv sock buffer 0 num [] in
	Bytes.sub buffer 0 (min l num)

(* [decode_img buf fname] outputs the bytes into [fname] *)
let decode_img (buf:bytes) (fname:string): string =
  let ch = open_out_bin fname in
  output_bytes ch buf;
  close_out ch;
  fname
(* return image path for now! *)

    (* "4 43 -77 42 -76 256 256" *)
let decode_service4 sock req =
  let res = let _ = send_info sock req in recv_info sock in
  let sep = String.index res '$' in
  (* let meta = String.sub res 0 sep in *)
  let img_str = String.sub res (sep+1) (String.length res - sep - 1) in
  (* let meta_l = String.split_on_char ' ' meta in *)
  let img_bts = Bytes.of_string img_str in
  decode_img img_bts "test.png"
