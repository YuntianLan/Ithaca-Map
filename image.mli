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

  (* [build_full_map res] is the file path  a single png that covers the
   * area discribed by [res]
   * returns "error" if no image can be built from [res] *)
  val build_full_map : result -> string
end

module Images : MapImage
