module type MapImage = sig
  (* [t] is the type of MapImage *)
  type t

  (* [params] is the type of query parameters for getting the map *)
  type params = {
    param_upleft_lon: float;
    param_upleft_lat: float;
    param_lowright_lon: float;
    param_lowright_lat: float;
    width: float;
    height: float;
  }

  (* [result] is the type of map image information *)
  type result = {
    img_grid: string list list;
    res_upleft_lon: float;
    res_upleft_lat: float;
    res_lowright_lon: float;
    res_lowright_lat: float;
    tree_depth: int;
    status: bool;
  }

  (* [init ()] takes in the root image
   * where the graphs are stored in the same folder and builds
   * the MapImage with all the graphs placed correctly in the
   * MapImage. *)
  val init : unit -> t

  (* [query_image t params] is the map image information that corresponds to the query
   * information [params] *)
  val query_image : t -> params -> result
end

module Images : MapImage

