(* ========= constants ========== *)
let max_depth = 6
let root_upleft_lon = -76.5527
let root_upleft_lat = 42.4883
let root_lowright_lon = -76.4649
let root_lowright_lat = 42.4235
let tile_size = 256.0

(* =========================== *)

module type MapImage = sig
  type t
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
    res_upleft_lon: float;
    res_upleft_lat: float;
    res_lowright_lon: float;
    res_lowright_lat: float;
    tree_depth: int;
    status: bool;
  }
  val init : unit -> t
  val query_image : t -> params -> result
end

module MapImage = struct
  type qnode = {
    upleft_lon: float;
    upleft_lat: float;
    lowright_lon: float;
    lowright_lat: float;
    img: string;
    depth: int;
  }

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
    res_upleft_lon: float;
    res_upleft_lat: float;
    res_lowright_lon: float;
    res_lowright_lat: float;
    tree_depth: int;
    status: bool;
  }

  type quadtree =
    | Leaf
    | Node of qnode * quadtree * quadtree * quadtree * quadtree

  type t = {
    image_qt:quadtree;
    all_lon_dpp:float list;
  }

  let make_singlenode_tree (qn:qnode) : quadtree =
    Node (qn, Leaf, Leaf, Leaf, Leaf)

  let make_children (qn:qnode) : quadtree list =
    let trunc s =
      Str.global_replace (Str.regexp "\\.png$") "" s in
    let upleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = "tiles/root.png" then "tiles/0.png" else (trunc qn.img)^"0.png";
      depth = qn.depth + 1;
    } in
    let upright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = "tiles/root.png" then "tiles/1.png" else (trunc qn.img)^"1.png";
      depth = qn.depth + 1;
    } in
    let lowleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = "tiles/root.png" then "tiles/2.png" else (trunc qn.img)^"2.png";
      depth = qn.depth + 1;
    } in
    let lowright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = "tiles/root.png" then "tiles/3.png" else (trunc qn.img)^"3.png";
      depth = qn.depth + 1;
    } in
    [make_singlenode_tree upleft; make_singlenode_tree upright;
     make_singlenode_tree lowleft; make_singlenode_tree lowright]

  let rec make_quadtree (qt:quadtree) : quadtree =
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
      depth = 0;
    } in
    root_node |> make_singlenode_tree |> make_quadtree


  (* returns true if the image stored in root node of [qt] intersects with the query box,
  false otehrwise*)
  let intersects (qt:quadtree) (params:params) : bool =
    match qt with
    | Leaf -> false
    | Node (qn,_,_,_,_) ->
      not (qn.lowright_lon <= params.param_upleft_lon
           || qn.upleft_lon >= params.param_lowright_lon
           || qn.lowright_lat >= params.param_upleft_lat
           || qn.upleft_lat <= params.param_lowright_lat)

  (* [is_valid_lon_dpp qt query_lon_dpp] is true if [qt] is at the deepest level or
   * its longitudinal distance per pixel (LonDPP) is less than or equal to [query_lon_dpp],
   * false otherwise *)
  let is_valid_lon_dpp (qn:qnode) (query_lon_dpp : float) : bool =
    let lon_dpp = (qn.lowright_lon -. qn.upleft_lon) /. tile_size in
    lon_dpp < query_lon_dpp

  let calc_all_lon_dpp (qt:quadtree) : float list =
    let rec helper (qt:quadtree) (acc:float list) =
      match qt with
      | Node(qn,c,_,_,_) ->
        let lon_dpp = (qn.lowright_lon -. qn.upleft_lon) /. tile_size in
        helper c (lon_dpp::acc)
      | Leaf -> acc in
    List.rev (helper qt [])

  let find_optimal_depth (all_lon_dpp:float list) (query_lon_dpp : float) : int =
    let rec helper dpplist =
      match dpplist with
      | [] -> max_depth
      | h::t ->
        if h <= query_lon_dpp then
          max_depth - (List.length t)
        else
          helper t in
    helper all_lon_dpp



  let rec image_queue (qt:quadtree) (depth:int) (params:params) (acc:qnode list) : qnode list =
    if depth = 0 then
      match qt with
      | Node(qn,_,_,_,_) -> qn::acc
      | Leaf -> acc
    else
      match qt with
      | Node(qn,ul,ur,ll,lr) ->
        List.fold_left (fun acc child ->
            if intersects child params then image_queue child (depth-1) params acc
            else
              acc) acc [ul;ur;ll;lr]
      | Leaf -> acc

  let qn_comparator_by_lat (qn1:qnode) (qn2:qnode):int =
    let diff = qn1.upleft_lat -. qn2.upleft_lat in
    if diff > 0. then 1
    else if diff = 0. then 0
    else -1

  let qn_comparator_by_lon (qn1:qnode) (qn2:qnode):int =
    let diff = qn1.upleft_lon -. qn2.upleft_lon in
    if diff > 0. then 1
    else if diff = 0. then 0
    else -1

  let sort_grid (qn_list:qnode list) : qnode list list =
    let sorted_by_lat = List.sort qn_comparator_by_lat qn_list in
    let fold_f (acc:qnode list list) (i:qnode) =
      match acc with
      | [[]] -> [[i]]
      | _ ->
        begin
          match List.rev acc with
          | [] -> failwith "impossible"
          | h::t ->
            if (qn_comparator_by_lat i (List.hd h)) = 0 then
              (i::h)::t |> List.rev
            else
              [i]::(List.rev acc) |> List.rev
        end in
    let rows = List.fold_left fold_f [[]] sorted_by_lat in
    List.map (List.sort qn_comparator_by_lon) rows

  let init () : t =
    let image_qt = init_quadtree "tiles/root.png" in
    {
      image_qt = image_qt;
      all_lon_dpp = calc_all_lon_dpp image_qt;
    }
  let query_image (map_image:t) (params:params) : result =

    let qt = map_image.image_qt in
    let query_upleft_lon = params.param_upleft_lon in
    let query_lowright_lon = params.param_lowright_lon in
    let w = params.width in
    let query_lon_dpp = (query_lowright_lon -. query_upleft_lon) /. w in
    let opt_depth = find_optimal_depth map_image.all_lon_dpp query_lon_dpp in
    let queue = image_queue qt opt_depth params [] in
    let sorted_rows = sort_grid queue in
    let get_img (qnlist:qnode list) : string list= List.map (fun i -> i.img) qnlist in
    let grid = List.map get_img sorted_rows in

    {
      img_grid = grid;
      res_upleft_lon = (sorted_rows |> List.hd |> List.hd).upleft_lon;
      res_upleft_lat = (sorted_rows |> List.hd |> List.hd).upleft_lat;
      res_lowright_lon = (sorted_rows |> List.rev |> List.hd |> List.hd).lowright_lon;
      res_lowright_lat = (sorted_rows |> List.rev |> List.hd |> List.hd).lowright_lat;
      tree_depth = opt_depth;
      status = true;
    }

end
