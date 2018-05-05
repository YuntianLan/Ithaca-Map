open Js_of_ocaml
open Js_of_ocaml_lwt
open Js
open Lwt

module Html = Dom_html
let js = Js.string
let doc = Html.document

(* ========= constants ========== *)
let base_url = "http://127.0.0.1:8000/"
let max_depth = 6
let min_depth = 1
let delta_zoom = 0.04
let delta_base_move = 0.03
let init_upleft_lon = -76.5527
let init_upleft_lat = 42.4883
let init_lowright_lon = -76.4649
let init_lowright_lat = 42.4235
let safe_width = 1120
let safe_height = 800

type params = {
  upleft_lon: float;
  upleft_lat: float;
  lowright_lon: float;
  lowright_lat: float;
  width: float;
  height: float;
}

type marker = {
  lat : float;
  lon : float;
  mk_tx : float;
  mk_ty : float;
  element : Html.imageElement Js.t;
}

type client_state = {
  mutable params: params;
  mutable current_depth: int;
  mutable wdpp : float;
  mutable hdpp : float;
  mutable tx : float;
  mutable ty: float;
  mutable rtx : float;
  mutable rty : float;
  mutable img_w : float;
  mutable img_h : float;
  mutable ullon_bound : float;
  mutable ullat_bound : float;
  mutable lrlon_bound : float;
  mutable lrlat_bound : float;
  mutable markers : marker list;
}

type http_res = {
  mutable by_coord : float*float;
  mutable by_name : (float*float) list;
  mutable route : float*((float*float) list);
  mutable img_path : string;
  mutable autocomplete : string list;
}



(* ========= HTTP requests ========== *)
let http_get url =
  XmlHttpRequest.get url >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let http_get_node_by_coord lat lon http_res =
  let url = base_url^"?index=1"^"&lat="^lat^"&lon="^lon in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ' ' res in
        let res_lat = List.nth params 0 |> float_of_string in
        let res_lon = List.nth params 1 |> float_of_string in
        http_res.by_coord <- (res_lat, res_lon);
        Lwt.return ()) in
  ignore(start ())

(* [split_coord_list s] is the list of coordinate tuples parsed from [s]
 * requries: [s] must be in the form "coord1,coord2;coord3,coord4;..."*)
let split_coord_list (s:string) : (float*float) list =
  let params = String.split_on_char ';' s in
  let tups = List.map (fun i ->
      let coords = String.split_on_char ',' i in
      let res_lat = List.nth coords 0 |> float_of_string in
      let res_lon = List.nth coords 1 |> float_of_string in
      (res_lat, res_lon)
    ) params in
  tups

let http_get_nodes_by_name name http_res =
  let url = base_url^"?index=2"^"&name="^name in
  let start () =
    http_get url >>= (fun res ->
        http_res.by_name <- split_coord_list res;
        Lwt.return ()) in
  ignore(start ())

let http_get_route (drive:bool) slat slon elat elon http_res =
  let url = base_url^"?index=3"^"&drive="^(string_of_bool drive)^"&slat="
            ^slat^"&slon="^slon^"&elat="^elat^"&elon="^elon in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ' ' res in
        let length = List.nth params 0 |> float_of_string in
        let coord_params = List.nth params 1 in
        let tups = split_coord_list coord_params in
        http_res.route <- (length, tups);
        Lwt.return ()) in
  ignore(start ())

let http_get_res (params:params) st http_res =
  let url = base_url^"?index=4"^
            "&upleft_lat"^string_of_float params.upleft_lat^
            "&upleft_lon"^string_of_float params.upleft_lon^
            "&lowright_lat"^string_of_float params.lowright_lat^
            "&lowright_lon"^string_of_float params.lowright_lon^
            "&width"^string_of_float params.width^
            "&height"^string_of_float params.height in
  let start () =
    http_get url >>= (fun res ->
        let nopng = String.sub res 0 (String.length res - 4) in
        let params = String.split_on_char '_' nopng in
        st.ullon_bound <- List.nth params 0 |> float_of_string;
        st.ullat_bound <- List.nth params 1 |> float_of_string;
        st.lrlon_bound <- List.nth params 2 |> float_of_string;
        st.lrlat_bound <- List.nth params 3 |> float_of_string;
        st.current_depth <- List.nth params 4 |> int_of_string;
        st.img_w <- List.nth params 5 |> float_of_string;
        st.img_h <- List.nth params 6 |> float_of_string;
        http_res.img_path <- res;
        Lwt.return ()) in
  ignore(start ())


let http_get_autocomp (s:string) (http_res:http_res) string list =
  let url = base_url^"?index=6"^"&input="^s in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ';' res in
        http_res.autocomplete <- params;
        Lwt.return ()) in
  ignore(start ())


let real_lrlat st =
  st.params.upleft_lat -. (st.hdpp *. st.params.height)

let real_lrlat st =
  st.params.upleft_lon +. (st.wdpp *. st.params.width)

let shift_left (delta:float) st =
  let new_params = {
    st.params with upleft_lon = st.params.upleft_lon -. delta;
                   lowright_lon = st.params.lowright_lon -. delta;
  } in
  st.params <- new_params

let shift_right (delta:float) st =
  let new_params = {
    st.params with upleft_lon = st.params.upleft_lon +. delta;
                   lowright_lon = st.params.lowright_lon +. delta;
  } in
  st.params <- new_params

let shift_up (delta:float) st =
  let new_params = {
    st.params with upleft_lat = st.params.upleft_lat +. delta;
                   lowright_lat = st.params.lowright_lat +. delta;
  } in
  st.params <- new_params

let shift_down (delta:float) st =
  let new_params = {
    st.params with upleft_lat = st.params.upleft_lat -. delta;
                   lowright_lat = st.params.lowright_lat -. delta;
  } in
  st.params <- new_params

let remove_markers (marker_dom : Html.divElement Js.t) st =
  let _ = List.map (fun mk -> Dom.removeChild marker_dom mk.element) st.markers in
  st.markers <- []

let update_markers st =
  let new_markers =
    List.map (fun mk ->
        {
          mk with mk_tx = (mk.lon -. st.params.upleft_lon) /. st.wdpp -.7. -. st.tx;
                  mk_ty = (st.params.upleft_lat -. mk.lat) /. st.hdpp -.7. -. st.ty;
        }
      ) st.markers in
  st.markers <- new_markers
