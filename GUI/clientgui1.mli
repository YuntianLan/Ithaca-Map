open Js_of_ocaml

module Html = Dom_html

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
  mutable ty: float;
  (* mutable rtx : float;
  mutable rty : float; *)
  mutable img_w : float;
  mutable img_h : float;
  mutable ullon_bound : float;
  mutable ullat_bound : float;
  mutable lrlon_bound : float;
  mutable lrlat_bound : float;
  mutable markers : marker list;
}

val http_get_node_by_coord : float -> float -> float*float

val http_get_nodes_by_name : string -> (float*float) list

val http_get_route : bool -> float -> float -> float -> float -> float*((float*float) list)

val http_get_res : params -> client_state -> string

val http_get_autocomp : string -> string list



(* Client state related functions *)
val update_img : client_state -> string
val zoom_in : client_state -> unit

val zoom_out : client_state -> unit

val on_drag : int -> int -> client_state -> unit
