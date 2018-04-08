(* constants *)
let max_depth = 18
let root_upleft_lon = -76.5192
let root_upleft_lat = 42.4569
let root_lowright_lon = -76.4676
let root_lowright_lat = 42.4301

(* =========================== *)

module type MapImage = sig
  type t
  type params
  type result
  val init : string -> t
  val get_map : params -> result
end

type qnode = {
  upleft_lon: float;
  upleft_lat: float;
  lowright_lon: float;
  lowright_lat: float;
  img: string;
  depth: int;
}
type quadtree =
  | Leaf
  | Node of qnode * quadtree * quadtree * quadtree * quadtree

module MapImage = struct

  type t = quadtree
  type params = {
    param_upleft_lon: float;
    param_upleft_lat: float;
    param_lowright_lon: float;
    param_lowright_lat: float;
    width: float;
    height: float;
  }
  type result = {
    img_grid: string list list;
    map_upleft_lon: float;
    map_upleft_lat: float;
    map_lowright_lon: float;
    map_lowright_lat: float;
    tree_depth: int;
    status: bool;
  }


  let make_singlenode_tree qn =
    Node (qn, Leaf, Leaf, Leaf, Leaf)

  let make_children qn =
    let trunc s =
      Str.global_replace (Str.regexp "\\.png$") "" s in
    let upleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = "img/root.png" then "img/1.png" else (trunc qn.img)^"1.png";
      depth = qn.depth + 1;
    } in
    let upright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = "img/root.png" then "img/2.png" else (trunc qn.img)^"2.png";
      depth = qn.depth + 1;
    } in
    let lowleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = "img/root.png" then "img/3.png" else (trunc qn.img)^"3.png";
      depth = qn.depth + 1;
    } in
    let lowright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = "img/root.png" then "img/4.png" else (trunc qn.img)^"4.png";
      depth = qn.depth + 1;
    } in
    [make_singlenode_tree upleft; make_singlenode_tree upright;
     make_singlenode_tree lowleft; make_singlenode_tree lowright]

  let rec make_quadtree qt =
    match qt with
    | Leaf -> Leaf
    | Node (qn,_,_,_,_) when qn.depth = max_depth -> qt
    | Node (qn,_,_,_,_) -> begin
      match make_children qn with
        | ul::ur::ll::lr::[] -> Node (qn,make_quadtree ul, make_quadtree ur,
                                      make_quadtree ll, make_quadtree lr)
        | _ -> failwith "invalid children"
    end

  let init_quadtree s =
    let root_node = {
      upleft_lon = root_upleft_lon;
      upleft_lat = root_upleft_lat;
      lowright_lon = root_lowright_lon;
      lowright_lat = root_lowright_lat;
      img = s;
      depth = 1;
    } in
    root_node |> make_singlenode_tree |> make_quadtree

	let get_map params =
	 failwith "Unimplemented"

end
