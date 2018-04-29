open Camlimages
open Png
(* ========= constants ========== *)
let image_folder = "tiles"^Filename.dir_sep
(* let image_folder = "tiles_test"^Filename.dir_sep *)
let max_depth = 6
(* let max_depth = 2 *)
let root_upleft_lon = -76.5527
let root_upleft_lat = 42.4883
let root_lowright_lon = -76.4649
let root_lowright_lat = 42.4235
let tile_size = 256.0

(* ============================== *)

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

  (* [build_full_map res] is the filepath of a single png that covers the
   * area discribed by [res]
   * returns "error" if no image can be built from [res] *)
  val build_full_map : result -> string

end


module Images : MapImage = struct
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

  let build_full_map r = ()

  (* [make_singlenode_tree qn] is the quadtree with node [qn] and children are Leaf*)
  let make_singlenode_tree (qn:qnode) : quadtree =
    Node (qn, Leaf, Leaf, Leaf, Leaf)

  (* [make_children qn] makes a quadtree with [qn] as its root node and children
   * accodring their spatial relations *)
  let make_children (qn:qnode) : quadtree list =
    let trunc s =
      Str.global_replace (Str.regexp "\\.png$") "" s in
    let upleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = image_folder^"root.png" then image_folder^"0.png" else (trunc qn.img)^"0.png";
      depth = qn.depth + 1;
    } in
    let upright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      img = if qn.img = image_folder^"root.png" then image_folder^"1.png" else (trunc qn.img)^"1.png";
      depth = qn.depth + 1;
    } in
    let lowleft = {
      upleft_lon = qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = image_folder^"root.png" then image_folder^"2.png" else (trunc qn.img)^"2.png";
      depth = qn.depth + 1;
    } in
    let lowright = {
      upleft_lon = (qn.lowright_lon -. qn.upleft_lon)/.2. +. qn.upleft_lon;
      upleft_lat = (qn.lowright_lat -. qn.upleft_lat)/.2. +. qn.upleft_lat;
      lowright_lon = qn.lowright_lon;
      lowright_lat = qn.lowright_lat;
      img = if qn.img = image_folder^"root.png" then image_folder^"3.png" else (trunc qn.img)^"3.png";
      depth = qn.depth + 1;
    } in
    [make_singlenode_tree upleft; make_singlenode_tree upright;
     make_singlenode_tree lowleft; make_singlenode_tree lowright]

  (* returns: [make_quadtree qt] recursively grows from the quadtree [qt] untill it reaches
   * the max_depth*)
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

  (* returns :[init_quadtree s] initializes a quadtree with [s] as the root image *)
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


  (* returns: [calc_all_lon_dpp qt] is the longitudinal distance per pixel (LonDPP)
   * for all depths *)
  let calc_all_lon_dpp (qt:quadtree) : float list =
    let rec helper (qt:quadtree) (acc:float list) =
      match qt with
      | Node(qn,c,_,_,_) ->
        let lon_dpp = (qn.lowright_lon -. qn.upleft_lon) /. tile_size in
        helper c (lon_dpp::acc)
      | Leaf -> acc in
    List.rev (helper qt [])


  (* [find_optimal_depth all_lon_dpp query_lon_dpp] is greatest depth whose
   * longitudinal distance per pixel (LonDPP) is less than or equal to
   * [query_lon_dpp] *)
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


  (* [image_queue qt depth params acc] is the list of qnodes at [depth] of
   * [qt] that covers all of the area requested in [params] order is
   * unspecified  *)
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

  (* [qn_comparator_by_lat qn1 qn2] is 1 if [qn1]'s latitude is larger than
   that of [qn2], 0 if they are equal, -1 otherwise*)
  let qn_comparator_by_lat (qn1:qnode) (qn2:qnode):int =
    let diff = qn1.upleft_lat -. qn2.upleft_lat in
    if diff > 0. then 1
    else if diff = 0. then 0
    else -1

  (* [qn_comparator_by_lat qn1 qn2] is 1 if [qn1]'s longitude is larger than
     that of [qn2], 0 if they are equal, -1 otherwise*)
  let qn_comparator_by_lon (qn1:qnode) (qn2:qnode):int =
    let diff = qn1.upleft_lon -. qn2.upleft_lon in
    if diff > 0. then 1
    else if diff = 0. then 0
    else -1

  (* returns: [sort_grid qn_list] is the sorted 2D list of [qn_list] according to longitudes
   * and latitudes
   * example: [[img/13.png, img/14.png, img/23.png, img/24.png],
               [img/31.png, img/32.png, img/41.png, img/42.png],
               [img/33.png, img/34.png, img/43.png, img/44.png]] is the result of
   * a 1D list containing these unsorted images. Each nested list represents a row
   * of images, sorted in longitudinal order, and the rows are sorted in the
   * latitudinal order *)
  let sort_grid (qn_list:qnode list) : qnode list list =
    let sorted_by_lat_down_up = List.sort qn_comparator_by_lat qn_list in
    let fold_f (acc:qnode list list) (i:qnode) =
      match acc with
      | [[]] -> [[i]]
      | _ ->
        begin
          match acc with
          | [] -> failwith "impossible"
          | h::t ->
            if (qn_comparator_by_lat i (List.hd h)) = 0 then
              (i::h)::t
            else
              [i]::acc
        end in
    let rows = List.fold_left fold_f [[]] sorted_by_lat_down_up in
    List.map (List.sort qn_comparator_by_lon) rows

  (* [init ()] is an initial instance of MapImage *)
  let init () : t =
    let image_qt = init_quadtree (image_folder^"root.png") in
    {
      image_qt = image_qt;
      all_lon_dpp = calc_all_lon_dpp image_qt;
    }

  (* [query_image_helper map_image params] is the response map images for requested information
   * in [params]*)
  let query_image_helper (map_image:t) (params:params) : result =

    let qt = map_image.image_qt in
    let query_upleft_lon = params.param_upleft_lon in
    let query_lowright_lon = params.param_lowright_lon in
    let query_upleft_lat = params.param_upleft_lat in
    let query_lowright_lat = params.param_lowright_lat in
    if (query_lowright_lon <= root_upleft_lon
        || query_upleft_lon >= root_lowright_lon
        || query_lowright_lat >= root_upleft_lat
        || query_upleft_lat <= root_lowright_lat
        || query_lowright_lat > query_upleft_lat
        || query_lowright_lon < query_upleft_lon) then
      {
        img_grid = [];
        res_upleft_lon = 0.;
        res_upleft_lat = 0.;
        res_lowright_lon = 0.;
        res_lowright_lat = 0.;
        tree_depth = 0;
        status = false;
      }
    else
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
        res_lowright_lon = (sorted_rows |> List.rev |> List.hd |> List.rev |> List.hd).lowright_lon;
        res_lowright_lat = (sorted_rows |> List.rev |> List.hd |> List.rev |> List.hd).lowright_lat;
        tree_depth = opt_depth;
        status = true;
      }

  (* [query_image map_image params] is the response map images for requested information
   * in [params]*)
  let query_image (map_image:t) (params:params) : result =
    try
      query_image_helper map_image params
    with
    | _ ->
      {
        img_grid = [];
        res_upleft_lon = 0.;
        res_upleft_lat = 0.;
        res_lowright_lon = 0.;
        res_lowright_lat = 0.;
        tree_depth = 0;
        status = false;
      }

  let get_img_rgb24 (img_path:string) : Rgb24.elt array array =
    let img_rgb24 = Png.load_as_rgb24 img_path [] in
    match img_rgb24 with
    | Rgb24 bmp ->
      let w = bmp.Rgb24.width in
      let h = bmp.Rgb24.height in
      Array.init h (fun j ->
          Array.init w (fun i ->
            Rgb24.get bmp i j))
    | _ -> failwith "Only supports RGB24"

  let build_full_map_rgb (img_grid:string list list) : Rgb24.elt array array =
    let int_tilesize = int_of_float tile_size in
    let fullimg_h = (img_grid |> List.length) * int_tilesize in
    let dummy_rgb = {Color.r = 0; g = 0; b = 0} in
    let buffer = Array.make fullimg_h (Array.make 0 dummy_rgb) in
    List.iteri (fun j imglst ->
        List.iteri (fun i img_path_i ->
            let rgb_elm = get_img_rgb24 img_path_i in
            Array.iteri (fun rgb_j row_of_rgb ->
                buffer.(j*int_tilesize+rgb_j) <- Array.append buffer.(j*int_tilesize+rgb_j) row_of_rgb)
              rgb_elm) imglst) img_grid;
    buffer

  let build_full_map (res:result) =
    let img_grid = res.img_grid in
    if (res.status = false || List.length img_grid = 0
        || img_grid |> List.hd |> List.length = 0) then
      "error"
    else
      let int_tilesize = int_of_float tile_size in
      let fullimg_h = (img_grid |> List.length) * int_tilesize in
      let fullimg_w = (img_grid |> List.hd |> List.length) * int_tilesize in
      let full_map_rgb = build_full_map_rgb img_grid in
      let buffer_rgb24 = Rgb24.create fullimg_w fullimg_h in
      for j = 0 to (fullimg_h-1) do
        for i = 0 to (fullimg_w-1) do
          Rgb24.set buffer_rgb24 i j full_map_rgb.(j).(i)
        done
      done;
      (* let start = String.length image_folder in
      let ul = img_grid |> List.hd |> List.hd in
      let lr = img_grid |> List.rev |> List.hd |> List.rev |> List.hd in *)
      let save_path = "test.png" in
      Png.save save_path [] (Rgb24 buffer_rgb24);
      save_path
end
