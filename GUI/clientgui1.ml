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
  param_upleft_lon: float;
  param_upleft_lat: float;
  param_lowright_lon: float;
  param_lowright_lat: float;
  width: float;
  height: float;
}

type marker = {
  lat : float;
  lon : float;
  mk_tx : float;
  mk_ty : float;
  element : Html.buttonElement Js.t;
}

type client_state = {
  mutable params: params;
  mutable current_depth: int;
  mutable wdpp : float;
  mutable hdpp : float;
  mutable tx : float;
  mutable ty : float;
  (* mutable rtx : float;
  mutable rty : float; *)
  mutable img_w : float;
  mutable img_h : float;
  mutable ullon_bound : float;
  mutable ullat_bound : float;
  mutable lrlon_bound : float;
  mutable lrlat_bound : float;

  mutable markers : marker list;
  (* mutable route : (int * int) list; *)


}


let real_lrlat st =
  st.params.param_upleft_lat -. (st.hdpp *. st.params.height)

let real_lrlat st =
  st.params.param_upleft_lon +. (st.wdpp *. st.params.width)

let shift_left (delta:float) st =
  let new_params = {
    st.params with param_upleft_lon = st.params.param_upleft_lon -. delta;
                   param_lowright_lon = st.params.param_lowright_lon -. delta;
  } in
  st.params <- new_params

let shift_right (delta:float) st =
  let new_params = {
    st.params with param_upleft_lon = st.params.param_upleft_lon +. delta;
                   param_lowright_lon = st.params.param_lowright_lon +. delta;
  } in
  st.params <- new_params

let shift_up (delta:float) st =
  let new_params = {
    st.params with param_upleft_lat = st.params.param_upleft_lat +. delta;
                   param_lowright_lat = st.params.param_lowright_lat +. delta;
  } in
  st.params <- new_params

let shift_down (delta:float) st =
  let new_params = {
    st.params with param_upleft_lat = st.params.param_upleft_lat -. delta;
                   param_lowright_lat = st.params.param_lowright_lat -. delta;
  } in
  st.params <- new_params

let remove_markers (marker_dom : Html.divElement Js.t) st =
  let _ = List.map (fun mk -> Dom.removeChild marker_dom mk.element) st.markers in
  st.markers <- []

let update_markers st =
  let new_markers =
    List.map (fun mk ->
        {
          mk with mk_tx = (mk.lon -. st.params.param_upleft_lon) /. st.wdpp -.7. -. st.tx;
                  mk_ty = (st.params.param_upleft_lat -. mk.lat) /. st.hdpp -.7. -. st.ty;
        }
      ) st.markers in
  st.markers <- new_markers


let update_img st =
  let path = http_get_res st.params st in
  update_markers st;
  base_url^"?index=5?path="^path

let update_required st =
  st.params.param_upleft_lon < st.ullon_bound ||
  st.params.param_upleft_lat > st.ullat_bound ||
  st.params.param_lowright_lon > st.lrlon_bound ||
  st.params.param_lowright_lat < st.lrlat_bound




let on_drag dx dy st = ()
  



let zoom_in st = 
  if st.current_depth = max_depth then () else
  let ullon = st.params.param_upleft_lon in
  let ullat = st.params.param_upleft_lat in
  let lrlon = st.params.param_lowright_lon in
  let lrlat = st.params.param_lowright_lat in
  let orig_width = st.params.width in
  let orig_height = st.params.height in
  let delta_lon = (lrlon -. ullon) /. 4. in
  let delta_lat = (ullat -. lrlat) /. 4. in
  let new_params = {
    param_upleft_lon    =   ullon +. delta_lon;
    param_upleft_lat    =   ullat -. delta_lat;
    param_lowright_lon  =   lrlon -. delta_lon;
    param_lowright_lat  =   lrlat +. delta_lat;
    width               =   orig_width;
    height              =   orig_height;
  } in
  st.params <- new_params;
  let img_path = http_get_res st.params st in
  ()

let zoom_out st =
  if st.current_depth = min_depth then () else
  let ullon = st.params.param_upleft_lon in
  let ullat = st.params.param_upleft_lat in
  let lrlon = st.params.param_lowright_lon in
  let lrlat = st.params.param_lowright_lat in
  let orig_width = st.params.width in
  let orig_height = st.params.height in
  let delta_lon = (lrlon -. ullon) /. 2. in
  let delta_lat = (ullat -. lrlat) /. 2. in
  let new_params = {
    param_upleft_lon    =   ullon -. delta_lon;
    param_upleft_lat    =   ullat +. delta_lat;
    param_lowright_lon  =   lrlon +. delta_lon;
    param_lowright_lat  =   lrlat -. delta_lat;
    width               =   orig_width;
    height              =   orig_height;
  } in
  st.params <- new_params;
  let img_path = http_get_res st.params st in
  ()









