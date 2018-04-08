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
    upleft_lon: float;
    upleft_lat: float;
    lowright_lon: float;
    lowright_lat: float;
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
	let init s =
		failwith "Unimplemented"

	let get_map params =
	 failwith "Unimplemented"

end
