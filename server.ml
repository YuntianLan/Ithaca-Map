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


let sip = Unix.inet_addr_of_string "127.0.0.1"
let sport = 4996


let init_server () =
  let gr = MapGraph.init_graph "graph/full.json" in
  let tr = ImageTree.init () in
  {imagetree = tr; mapgraph = gr}


(* [lst2str lst] converts the string list to the string
 * representation of the list *)
let lst2str lst =
  let rec helper l acc start =
    match l with
    | [] -> if (not start) then acc ^ "]" else "[]"
    | h::t ->
      let nacc = if start then "[" ^ h
        else (acc ^ ";" ^ h) in
      helper t nacc false in
  helper lst "" true

(* [slst2str lst] does similar things to lst2str, except
 * the target is a string list list *)
let slst2str lst =
  let rec helper l acc start =
    match l with
    | [] -> if (not start) then acc ^ "]" else "[]"
    | h::t ->
      let nacc = if start then "[" ^ (lst2str h)
        else (acc ^ ";" ^ (lst2str h)) in
      helper t nacc false in
  helper lst "" true



(* [responst_bytes] tg s takes in the tree/graph and
 * the request string, respond with a stream of bytes *)
let response_bytes tg s =
  let args = String.split_on_char ' ' s in
  let idx = List.hd args in
  if idx = "1" then
    let res = try
        if (List.length args) < 3 then
          Bytes.of_string
            "Error: too few arguments for service 1"
        else
          let lat = float_of_string (List.nth args 1) in
          let lon = float_of_string (List.nth args 2) in
          let nd =
            MapGraph.get_node_by_coord lat lon tg.mapgraph in
          let rlat, rlon = MapGraph.node_to_coord nd in
          let slat = string_of_float rlat in
          let slon = string_of_float rlon in
          Bytes.of_string (slat ^ " " ^ slon)
      with _ ->
        Bytes.of_string "Error: invalid command for service 1"
    in res
    (* Location name to node coord *)
  else if idx = "2" then
    let res = try
        if (String.length s) < 3 then Bytes.of_string
            "Error: string length for service 2 too short"
        else
          let name = String.sub s 2 ((String.length s)-2) in
          match MapGraph.get_node_by_name name tg.mapgraph with
          | None -> Bytes.of_string "Error: location not found"
          | Some nd ->
            let lat, lon = MapGraph.node_to_coord nd in
            Bytes.of_string
              ((string_of_float lat) ^ " " ^ (string_of_float lon))
      with _ ->
        Bytes.of_string "Error: invalid command for service 2"
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
      Bytes.of_string res_bts
    with
    | _ -> Bytes.of_string "illegal arguments for service 3"
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
        let res =
          Bytes.of_string
            "Error: exception in parsing parameter!" in
        res
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
        let _ = print_endline res1 in
        let _ = print_endline "bytes length:" in
        let _ = print_int (Bytes.length bts) in
        let res = Bytes.cat (Bytes.of_string res1) bts in
        res
    end
    (* Error *)
  else
    let res = "Error: improper request" in
    Bytes.of_string res






(* [handle_client tg desc] takes in the client connection
 * desc and respond to its requests until the connection
 * has ended. *)
let rec handle_client tg desc : unit =
  let str = Bytes.make 1024 ' ' in
  let rec get_len () =
    (* let _ = print_endline "waiting for response" in *)
    if Thread.wait_timed_read desc 0.5 then
      Unix.recv desc str 0 1024 []
    else
      (* let _ = print_endline "yielding to other threads" in *)
      let _ = Thread.yield () in
      get_len ()
  in
  let len = get_len () in
  let _ = print_endline "received message from client" in
  let s = Bytes.to_string (Bytes.sub str 0 len) in
  if ((s = "exit") || (s = "quit")) then
    let _ = Unix.close desc in
    Thread.exit ()
  else if s="test" then
    let _ = Unix.send desc
        (Bytes.create 1000000) 0 1000000 [] in
    handle_client tg desc
  else
    let res = response_bytes tg s in
    let _ = Unix.send desc res 0 (Bytes.length res) [] in
    handle_client tg desc








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

(* The temporary begin_service is a mini version of the
 * actual one in that it only allows one single connection. *)
let rec begin_service_temp tg =
  let server_socket =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _ = Unix.bind server_socket
      (Unix.ADDR_INET(sip,sport)) in
  let _ = Unix.listen server_socket 1 in
  let desc, addr = Unix.accept server_socket in
  let _ = print_endline "connected" in
  let thr = Thread.create (handle_client tg) desc in
  Thread.join thr;
  begin_service_temp tg



let _ = begin_service_temp (init_server ())
