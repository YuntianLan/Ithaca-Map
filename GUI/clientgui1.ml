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
  element : Html.buttonElement Js.t option;
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
