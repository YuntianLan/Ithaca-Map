open Js_of_ocaml
open Js

module Html = Dom_html
let js = Js.string
let doc = Html.document

(* ========= constants ========== *)
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
  params: params;
  current_depth: int;
  wdpp : float;
  hdpp : float;
  tx : float;
  ty: float;
  rtx : float;
  rty : float;
  img_w : float;
  img_h : float;
  markers : marker list;
}
(* [decode buf fname] outputs the bytes into [fname] *)
let decode (buf:bytes) (fname:string): unit =
  let ch = open_out_bin fname in
  output_bytes ch buf;
  close_out ch

let real_lrlat st =
  st.params.upleft_lat -. (st.hdpp *. st.params.height)

let real_lrlat st =
  st.params.upleft_lon +. (st.wdpp *. st.params.width)

let shift_left (delta:float) st =
  let new_params = {
    st.params with upleft_lon = st.params.upleft_lon -. delta;
                   lowright_lon = st.params.lowright_lon -. delta;
  } in
  {
    st with params = new_params;
            tx = st.tx -. (delta /. st.wdpp);
  }

let shift_right (delta:float) st =
  let new_params = {
    st.params with upleft_lon = st.params.upleft_lon +. delta;
                   lowright_lon = st.params.lowright_lon +. delta;
  } in
  {
    st with params = new_params;
            tx = st.tx +. (delta /. st.wdpp);
  }

let shift_up (delta:float) st =
  let new_params = {
    st.params with upleft_lat = st.params.upleft_lat +. delta;
                   lowright_lat = st.params.lowright_lat +. delta;
  } in
  {
    st with params = new_params;
            ty = st.ty +. (delta /. st.hdpp);
  }

let shift_down (delta:float) st =
  let new_params = {
    st.params with upleft_lat = st.params.upleft_lat -. delta;
                   lowright_lat = st.params.lowright_lat -. delta;
  } in
  {
    st with params = new_params;
            ty = st.ty -. (delta /. st.hdpp);
  }

let remove_markers (marker_dom : Html.divElement Js.t) st =
  let _ = List.map (fun mk -> Dom.removeChild marker_dom mk.element) st.markers in
  {
    st with markers = []
  }

let update_markers st =
  List.map (fun mk ->
      {
        mk with mk_tx = (mk.lon -. st.params.upleft_lon) /. st.wdpp -.7. -. st.tx;
                mk_ty = (st.params.upleft_lat -. mk.lat) /. st.hdpp -.7. -. st.ty;
      }
    ) st.markers
