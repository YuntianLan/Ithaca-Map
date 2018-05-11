open Js_of_ocaml

module Html = Dom_html

(* To be replaced with type from Image.ml *)
type params = {
  param_upleft_lon: float;
  param_upleft_lat: float;
  param_lowright_lon: float;
  param_lowright_lat: float;
  width: float;
  height: float;
}

(* The information of route *)
type node = {
  lat : float;
  lon : float;
  x   : int;
  y   : int;
}


type marker = {
  lat : float;
  lon : float;
  (* If marker is out of bounds, calculated x and y
    would be out of the state's window, can safely ignore *)
  win_x : int;
  win_y : int;

  (* mk_tx : float;
  mk_ty : float; *)

  element : Html.buttonElement Js.t;
}

type client_state = {
  mutable params: params;
  mutable current_depth: int;

  mutable wdpp : float;
  mutable hdpp : float;

  (* Parameters for the View to display the image *)
  mutable win_ul_x : int;
  mutable win_ul_y : int;
  mutable win_w : int;
  mutable win_h : int;

  mutable markers : marker list;
  mutable route_nodes : node list;



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
}

(* val http_get_node_by_coord : float -> float -> float*float

val http_get_nodes_by_name : string -> (float*float) list

val http_get_route : bool -> float -> float -> float -> float -> float*((float*float) list)

val http_get_res : params -> client_state -> string

val http_get_autocomp : string -> string list *)



(* Client state related functions *)

(* Calculate and update the client state in place
    when the zoom in button is pressed. *)
val zoom_in : client_state -> unit

(* Calculate and update the client state in place
    when the zoom out button is pressed. *)
val zoom_out : client_state -> unit

(* Calculate and update the client state in place
    when the map is dragged. *)
val on_drag : int -> int -> client_state -> unit










