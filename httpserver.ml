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
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Image
open Graph
open Camlimages
open Pervasives

(* Parameter keys:
 * index: int {1,2,3,4,5,6,7}
 * 		1. Nearest node coord given coord
 * 		2. Nearest node coord given location name
 * 		3. Path from one coord to another coord
 * 		4. Result given a query parameter
 * 		5. Image (png) given a result
 * 		6. List of auto-completed names given a string input
 * 		7. List of coordinates and names given location type
 *
 * Additional parameters:
 *
 * Parameters for index = 1:
 * -- lat: float, eg: 42.813746
 * -- lon: float, eg: -76.7116266
 *
 * Parameter for index = 2:
 * -- name: string, eg: Texas Roadhouse
 *
 * Parameters for index = 3:
 * -- drive: true / false
 * -- slat: lat for start, float
 * -- slon: lon for start, float
 * -- elat: lat for end, float
 * -- elon: lon for end, float
 *
 * Parameters for index = 4:
 * -- upleft_lat: float
 * -- upleft_lon: float
 * -- lowright_lat: float
 * -- lowright_lon: float
 * -- width: float
 * -- height: float
 *
 * Parameters for index = 5:
 * -- path: string
 *
 * Parameters for index = 6:
 * -- input: string
 * 
 * Parameters for index = 7:
 * -- type: string
 *
 *)

let eg_resp1 = "42.813646 -76.7116234"
let eg_resp2 = "42.813646,-76.7116234;42.813646,-76.7116234"
(* Service 3, first float represent length of route,
 * seperated by a space, the remaining coords have no space in between *)
let eg_resp3 = "3.45 42.813646,-76.7116234;42.813646,-76.7116234"

let eg_resp7 = "42.1,-76.3,texas roadhouse;42.1,-76.3,texas roadhouse"


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



let init_server () =
	let gr = MapGraph.init_graph "graph/full.json" in
	let tr = ImageTree.init () in
	{imagetree = tr; mapgraph = gr}



(* [responst_bytes] tg s takes in the tree/graph and
 * the request string, respond with a stream of bytes *)
let response_str tg uri =
	let idx = match Uri.get_query_param uri "index" with
	| None -> "0" | Some s -> s in

	(* Nearest node coord given coord *)
	if idx = "1" then
		let slat_opt = Uri.get_query_param uri "lat" in
		let slon_opt = Uri.get_query_param uri "lon" in
		begin match slat_opt, slon_opt with
		| Some slat, Some slon ->
			begin try
				let lat = float_of_string slat in
				let lon = float_of_string slon in
				let nd =
					MapGraph.get_node_by_coord lat lon tg.mapgraph in
				let rlat, rlon = MapGraph.node_to_coord nd in
				(string_of_float rlat) ^ " " ^ (string_of_float rlon)
			with _ -> "Error: idx = 1, exception in parsing uri"
			end
		| _, _ -> "Error: idx = 1, lat/lon not found"
		end

	(* Location name to node coord *)
	else if idx = "2" then
		let name_opt = Uri.get_query_param uri "name" in
		match name_opt with
		| None -> "Error: idx = 2, name not found"
		| Some name ->
			begin try
				begin match MapGraph.get_node_by_name name tg.mapgraph with
				| [] -> "[]"
				| nlst ->
					let floats = List.map MapGraph.node_to_coord nlst in
					let strs = List.map (fun (la,lo) ->
						(string_of_float la) ^ "," ^ (string_of_float lo)) floats in
					let accum a b = a ^ ";" ^ b in
					List.fold_left accum (List.hd strs) (List.tl strs)
				end
			with _ -> "Error: idx = 2, exception in parsing uri"
			end

	(* Path from one coord to another coord *)
	else if idx = "3" then
		let drive_opt = Uri.get_query_param uri "drive" in
		let slat_opt = Uri.get_query_param uri "slat" in
		let slon_opt = Uri.get_query_param uri "slon" in
		let elat_opt = Uri.get_query_param uri "elat" in
		let elon_opt = Uri.get_query_param uri "elon" in
		match drive_opt, slat_opt, slon_opt, elat_opt, elon_opt with
		| Some drive, Some slat, Some slon, Some elat, Some elon ->
			begin try
				let dflag = drive = "true" in
				let ns = MapGraph.get_node_by_coord
					(float_of_string slat) (float_of_string slon) tg.mapgraph in
				let ne = MapGraph.get_node_by_coord
					(float_of_string elat) (float_of_string elon) tg.mapgraph in
				let trip_len, path = MapGraph.find_path dflag ns ne tg.mapgraph in
				let floats = List.map MapGraph.node_to_coord path in
				let strs = List.map (fun (la,lo) ->
						(string_of_float la) ^ "," ^ (string_of_float lo)) floats in
				let accum a b = a ^ ";" ^ b in
				let total_str = List.fold_left accum (List.hd strs) (List.tl strs) in
				(string_of_float trip_len) ^ " " ^ total_str
			with _ -> "Error: idx = 3, exception in parsing uri"
			end
		| _, _, _, _, _ -> "Error: idx = 3, parameter not found"

	(* Result and image given client param *)
	else if idx = "4" then
		let ulat_opt = Uri.get_query_param uri "upleft_lat" in
		let ulon_opt = Uri.get_query_param uri "upleft_lon" in
		let llat_opt = Uri.get_query_param uri "lowright_lat" in
		let llon_opt = Uri.get_query_param uri "lowright_lon" in
		let width_opt = Uri.get_query_param uri "width" in
		let height_opt = Uri.get_query_param uri "height" in
		match ulat_opt, ulon_opt, llat_opt, llon_opt, width_opt, height_opt with
		| Some ulat, Some ulon, Some llat, Some llon, Some width, Some height ->
			begin try
				let prm : param = {
					param_upleft_lon = float_of_string ulon;
					param_upleft_lat = float_of_string ulat;
					param_lowright_lon = float_of_string llon;
					param_lowright_lat = float_of_string llat;
					width = float_of_string width;
					height = float_of_string height;
				} in
				let rt = ImageTree.query_image tg.imagetree prm in
				let path = ImageTree.build_full_map rt in
				path
			with _ -> "Error: idx = 4, exception in parsing uri"
			end
		| _, _, _, _, _, _ -> "Error: idx = 4, parameter not found"

	(* List of auto-completed names given a string input *)
	else if idx = "6" then
		let input_opt = Uri.get_query_param uri "input" in
		begin match input_opt with
		| None -> "Error: idx = 6, parameter 'input' not found"
		| Some input ->
			begin match MapGraph.autocomplete tg.mapgraph input with
			| [] -> "[]"
			| lst ->
				let accum a b = a ^ ";" ^ b in
				List.fold_left accum (List.hd lst) (List.tl lst)
			end
		end

	else if idx = "7" then
		begin try
			let tp = Uri.get_query_param uri "type" in
			begin match tp with
			| None -> "Error: idx = 7, parameter 'tp' not found"
			| Some str ->
				let category_of_string str =
					if str = "fooddrink" then 	Some Graph.FoodDrink 
					else if str = "shop" then 	Some Graph.Shop
					else if str = "study" then	Some Graph.Study
					else if str = "fuel" then	Some Graph.Fuel
					else if str = "other" then	Some Graph.Other
					else  None 
				in
				let lst = MapGraph.nodes_ways_oftype (category_of_string str) tg.mapgraph in
				let lst = List.map (fun (x,y,z) -> Printf.sprintf "%f,%f,%s" x y z) lst in 
				let ret = List.fold_left (fun acc s -> if acc = "" then s else acc^";"^s) "" lst in 
				ret 
			end
		with _ -> "Error: idx = 7, exception in parsing type"
		end
	else "Error: invalid service index"


let server =
	let tg = init_server () in
	let callback _conn req body =
		let uri = req |> Request.uri in
		body |> Cohttp_lwt.Body.to_string >|= (fun body -> ())
		>>= (fun body ->
			if (Uri.get_query_param uri "index") = Some "5" then
				match Uri.get_query_param uri "path" with
				| None ->
					Server.respond_string
					~headers:(Header.init_with "Access-Control-Allow-Origin" "*")
					~status:`OK ~body:("Error: idx = 5, no path provided") ()
				| Some path ->
					Server.respond_file
					~headers:(Header.init_with "Access-Control-Allow-Origin" "*")
					~fname: path ()
			else
			Server.respond_string
			~headers:(Header.init_with "Access-Control-Allow-Origin" "*")
			~status:`OK ~body:(response_str tg uri) ())
	in
	Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let _ = Lwt.async_exception_hook := ignore
let () = ignore (Lwt_main.run server)
