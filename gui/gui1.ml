open Js_of_ocaml
open Js_of_ocaml_lwt
open Js
open Clientgui1
open Lwt
(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html
let img_path = ref "unset"

let js = Js.string
let doc = Html.document
let base_url = "http://127.0.0.1:8000/"
let http_get url =
  XmlHttpRequest.get url >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())
(* ========= constants ========== *)
let max_depth = 6
let min_depth = 1
let delta_zoom = 0.04
let delta_base_move = 0.03
let init_upleft_lon = -76.5496
let init_upleft_lat = 42.4750
(* let init_lowright_lon = -76.4670
let init_lowright_lat = 42.4279 *)
let init_lowright_lon = -76.4670
let init_lowright_lat = 42.4279
type marker = {
  lat : float;
  lon : float;
  mk_tx : float;
  mk_ty : float;
  element : Html.buttonElement Js.t;
}
let markers1 : marker list ref = ref []
let markers2 : marker list ref = ref []

let start_marker = ref None
(* let buttonlist2 = ref []
let buttondisplay2 = ref [] *)
let end_marker = ref None
(* let dbstart = ref None
let dbend = ref None *)

let coordinates = [(12.0,14.0);(30.0,40.0);(60.0,8.0);(388.0,200.0)]

let route = ref (0.,[(0.,0.)])

let img_map = Html.createImg doc


(* Dummy mutable values *)
let by_coord = ref (0.,0.)
let by_name = ref [(0.,0.)]
let route = ref (0.,[(0.,0.)])
let img_path = ref ""
let autocomp = ref [""]



let param = {
  param_upleft_lon = init_upleft_lon;
  param_upleft_lat = init_upleft_lat;
  param_lowright_lon = init_lowright_lon;
  param_lowright_lat = init_lowright_lat;
  width = 0.;
  height = 0.;
}
let st = {
  params = param;
  current_depth = 0;
  wdpp = 0.;
  hdpp = 0.;
  tx = 0.;
  ty = 0.;
  (* mutable rtx : float;
     mutable rty : float; *)
  img_w = 0.;
  img_h  = 0.;
  ullon_bound = 0.;
  ullat_bound = 0.;
  lrlon_bound = 0.;
  lrlat_bound = 0.;
  markers = [];
}



(* ========= HTTP requests ========== *)
let http_get url =
  XmlHttpRequest.get url >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let http_get_node_by_coord lat lon =
  let url = base_url^"?index=1"^"&lat="^(string_of_float lat)^
            "&lon="^(string_of_float lon) in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ' ' res in
        let res_lat = List.nth params 0 |> float_of_string in
        let res_lon = List.nth params 1 |> float_of_string in
        by_coord := (res_lat, res_lon);
        Lwt.return ()) in
  ignore(start ());
  !by_coord

(* [split_coord_list s] is the list of coordinate tuples parsed from [s]
 * requries: [s] must be in the form "coord1,coord2;coord3,coord4;..."*)
let split_coord_list (s:string) : (float*float) list =
  let params = String.split_on_char ';' s in
  let tups = List.map (fun i ->
      let coords = String.split_on_char ',' i in
      let res_lat = List.nth coords 0 |> float_of_string in
      let res_lon = List.nth coords 1 |> float_of_string in
      (res_lat, res_lon)
    ) params in
  tups

let http_get_nodes_by_name id name coord_to_markers addbutton div_map_container =
  let url = base_url^"?index=2"^"&name="^name in
  let start () =
    http_get url >>= (fun res ->
        if id = 1 then
          markers1 := res |> split_coord_list |> coord_to_markers
        else
          markers2 := res |> split_coord_list |> coord_to_markers;
        addbutton div_map_container;
        Lwt.return ()) in
  ignore(start ())

let http_get_route (drive:bool) slat slon elat elon =
  let url = base_url^"?index=3"^"&drive="^(string_of_bool drive)^"&slat="
            ^(string_of_float slat)^"&slon="^(string_of_float slon)^"&elat="
            ^(string_of_float elat)^"&elon="^(string_of_float elon) in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ' ' res in
        let length = List.nth params 0 |> float_of_string in
        let coord_params = List.nth params 1 in
        let tups = split_coord_list coord_params in
        route := (length, tups);
        Lwt.return ()) in
  ignore(start ())




let http_get_autocomp (s:string) =
  let url = base_url^"?index=6"^"&input="^s in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ';' res in
        autocomp := params;
        Lwt.return ()) in
  ignore(start ());
  !autocomp

let http_get_res st callback canvas context =
  (* let _ = Dom_html.window##alert(js 
    ((st.params.param_lowright_lon |> string_of_float)
     ^ " " ^ (st.params.param_lowright_lat |> string_of_float))) in *)
  let url = base_url^"?index=4"^
            "&upleft_lat="^string_of_float st.params.param_upleft_lat^
            "&upleft_lon="^string_of_float st.params.param_upleft_lon^
            "&lowright_lat="^string_of_float st.params.param_lowright_lat^
            "&lowright_lon="^string_of_float st.params.param_lowright_lon^
            "&width="^string_of_float st.params.width^
            "&height="^string_of_float st.params.height in
  let _ = Dom_html.window##alert(js url) in
  let start () =
    http_get url >>= (fun res ->

        let nopng = String.sub res 0 (String.length res - 4) in
        let params = String.split_on_char '_' nopng in
        let zero_cache = List.nth params 0 in
        let ullon = String.sub zero_cache 6 (String.length zero_cache - 6) in
        st.ullon_bound <- ullon |> float_of_string;
        st.ullat_bound <- List.nth params 1 |> float_of_string;
        st.lrlon_bound <- List.nth params 2 |> float_of_string;
        st.lrlat_bound <- List.nth params 3 |> float_of_string;
        st.current_depth <- List.nth params 4 |> int_of_string;
        st.img_w <- List.nth params 5 |> float_of_string;
        st.img_h <- List.nth params 6 |> float_of_string;
        st.wdpp <- (st.lrlon_bound -. st.ullon_bound) /. st.img_w;
        st.hdpp <- (st.ullat_bound -. st.lrlat_bound) /. st.img_h;
        st.tx <- (st.params.param_upleft_lon -. st.ullon_bound) /. st.wdpp;
        st.ty <- ( st.ullat_bound -. st.params.param_upleft_lat) /. st.hdpp;
        let canvas_w = st.params.width in
        let canvas_h = st.params.height in

        let width = st.params.width in
        let height = st.params.height in

        let ullon = st.params.param_upleft_lon in
        let ullat = st.params.param_upleft_lat in
        let lrlon = st.params.param_upleft_lon +. st.wdpp *. width in
        let lrlat = st.params.param_upleft_lat -. st.hdpp *. height in

        let params_new = {
          param_upleft_lon = ullon;
          param_upleft_lat = ullat;
          param_lowright_lon = lrlon;
          param_lowright_lat = lrlat;
          width = width;
          height = height;
        } in

        st.params <- params_new;

        let ullon_temp = st.ullon_bound +. st.wdpp *. st.tx |> string_of_float in
        let ullat_temp = st.ullat_bound -. st.hdpp *. st.ty |> string_of_float in
        let lrlon_temp = st.ullon_bound +. st.wdpp *. st.tx +. st.wdpp *. canvas_w
          |> string_of_float in
        let lrlat_temp = st.ullat_bound -. st.hdpp *. st.ty -. st.hdpp *. canvas_h
          |> string_of_float in

        let swdpp = string_of_float st.wdpp in
        let shdpp = string_of_float st.hdpp in
        let swidth = string_of_float st.params.width in
        let sheight = string_of_float st.params.height in
        (* let _ = Dom_html.window##alert(js 
          (swdpp ^ " " ^ shdpp ^ " " ^ (string_of_int st.current_depth))) in
        let _ = Dom_html.window##alert(js 
          (swidth ^ " " ^ sheight)) in
        let _ = Dom_html.window##alert(js 
          (ullon_temp ^ " " ^ ullat_temp ^ " " ^ lrlon_temp ^ " " ^ lrlat_temp)) in *)

        let _ = Dom_html.window##alert(js
                                         (ullon_temp ^ " " ^ ullat_temp ^ " " ^ lrlon_temp ^ " " ^ lrlat_temp ^" "
                                          ^ (string_of_float st.wdpp)^ " "^(string_of_float st.hdpp)^" "^(string_of_float st.tx)^" "^(string_of_float st.ty))) in
        let _ = callback canvas context (js
          (base_url^"?index=5&path="^res)) (st.tx, st.ty) in

        (* img_path := res; *)
        Lwt.return ()) in
  ignore(start ())
(* ========= HTTP requests ========== *)

















let clear_start div =
  List.iter (fun x -> Dom.removeChild div x.element) (!markers1);
  (match !start_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  markers1 := [];
  start_marker := None


let clear_end div =
  List.iter (fun x -> Dom.removeChild div x.element) (!markers2);
  (match !end_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  markers2 := [];
  end_marker := None


let clear_all div =
  List.iter (fun x -> Dom.removeChild div x.element) (!markers1);
  List.iter (fun x -> Dom.removeChild div x.element) (!markers2);

  (match !start_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  (match !end_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  markers1 := [];
  markers2 := [];
  start_marker := None;
  end_marker := None

(* Set the class of an Html element *)
let setClass elt s = elt##className <- js s
(* Set the ID of an Html element *)
let setId elt s = elt##id <- js s

let append_text e s = Dom.appendChild e (doc##createTextNode (js s))

(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail

let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##width <- w; c##height <- h; c

let draw_line context lst =
  List.fold_left
    (fun acc cor  ->
       let prevX = (fst acc) in
       let prevY = (snd acc) in
       let x = fst cor in
       let y = snd cor in
       context##beginPath ();
       context##moveTo (prevX,prevY);
       context##lineTo (x,y);
       context##stroke ();
       context##closePath ();
       (x,y)
    )
    (List.nth lst 0) lst

let draw_background_with_line canvas context src offset lst =
  img_map##onload <- Html.handler
      (fun ev ->
         context##clearRect (0.0,0.0,(float_of_int canvas##width),
          (float_of_int canvas##height));
         context##drawImage_full (img_map, fst(offset), snd(offset),
          (float_of_int canvas##width), (float_of_int canvas##height),
          0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
         draw_line context lst;
         Js._false);
  (* setId img_map "map";
  img_map##src <- src; *)
  img_map


let draw_background canvas context src offset =
  img_map##onload <- Html.handler
      (fun ev ->
         context##clearRect (0.0,0.0,(float_of_int canvas##width),
          (float_of_int canvas##height));
         context##drawImage_full (img_map, fst(offset), snd(offset),
          (float_of_int canvas##width), (float_of_int canvas##height),
          0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
         Js._false);
  setId img_map "map";
  img_map##src <- src;
  img_map

let clear_background i canvas context =
  let img_map = i in
  img_map##onload <- Html.handler
      (fun ev ->
         context##clearRect (0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
         context##drawImage (img_map, (10.), (10.));
         Js._false)
(* close all autocomplete lists in the document*)
let closeAllList elt input =
  Js.Opt.iter input (fun inp ->
      match elt with
      | None -> let element =  (Html.createDiv doc) in
        let x = doc##getElementsByClassName (js "autocomplete-items") in
        for i = 0 to x##length - 1 do
          Js.Opt.iter (x##item (i))
            (fun i ->
               if(element <> i && element <> inp)
               then Js.Opt.iter (i##parentNode) (fun x -> Dom.removeChild x i))
        done
      | Some element ->
        let x = doc##getElementsByClassName (js "autocomplete-items") in
        for i = 0 to x##length - 1 do
          Js.Opt.iter (x##item (i))
            (fun i -> if(element <> i && element <> inp)
              then Js.Opt.iter (i##parentNode) (fun x -> Dom.removeChild x i))
        done
    )


let autocomplete textbox =
  let currentFocus = ref 0 in
  textbox##oninput <- Html.handler
      (fun _ ->
         closeAllList None (Dom_html.CoerceTo.element textbox);
         doc##onclick <- Html.handler (fun ev -> (closeAllList
          (Js.Opt.to_option ev##target) (Dom_html.CoerceTo.element textbox));Js._true);
         (* Create a new div to contain all the relevant autocomplete item *)
         let a = Html.createDiv doc in
         let v = Js.to_string textbox##value in
         let lst = http_get_autocomp v in
         (* Dom_html.window##alert (js v); *)

         currentFocus := -1;

         (* let newDiv = Html.createDiv doc in *)
         setId a (Js.to_string textbox##id^ "autocomplete-list");
         setClass a "autocomplete-items";
         (match Js.Opt.to_option textbox##parentNode with
          | None -> failwith "error"
          | Some x -> Dom.appendChild x a);
         (* Dom.appendChild div_card_content a; *)


         for i = 0 to List.length lst - 1 do
           let word = List.nth lst i in
           (* Dom_html.window##alert (js word); *)
           if(String.(sub word 0 (length v) |> uppercase_ascii) = String.uppercase_ascii v)
           (* create a DIV element for each matching element: *)
           then let b = ref (Html.createDiv doc) in
             (* make the matching letters bold: *)
             let inn = "<strong>" ^ (String.sub word 0 (String.length v)) ^ "</strong>" ^
                       (String.sub word (String.length v) (String.length word-String.length v))^
                       "<input type='hidden' value='" ^ word ^ "'>" in
             !b##innerHTML <- js inn;
             (* execute a function when someone clicks on the item value (DIV element): *)
             (* When one of the suggested text is clicked, change the input to that word. *)
             !b##onclick <- Html.handler
                 (fun _ ->
                    (* returns a Dom.nodeList *)
                    let inputfield = (!b)##getElementsByTagName (js "input") in
                    (* the first item of the list *)
                    let firstone = inputfield##item (0) in
                    Js.Opt.iter firstone
                      (fun node ->
                         let elt = Dom_html.CoerceTo.element node in
                         Js.Opt.iter elt
                           (fun elt ->
                              let input = Dom_html.CoerceTo.input elt in
                              Js.Opt.iter input
                                (fun i ->
                                   (* change the value in the textbox to the clicked text *)
                                   let content = i##value in
                                   textbox##value <- content
                                )
                           )
                      )
                  ;Js._true
                 );
             Dom.appendChild a !b
         done;
         Js._true)

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f


let get_geo () =
  if (Geolocation.is_supported()) then
    let geo = Geolocation.geolocation in
    let options = Geolocation.empty_position_options() in
    let () = options##enableHighAccuracy <- true in
    let f_success pos =
      let coords = pos##coords in
      let latitude = coords##latitude in
      (* Firebug.console##debug(latitude); *)
      Dom_html.window##alert (js ((string_of_float latitude)^(string_of_float coords##longitude)));
    in
    let f_error err =
      let code = err##code in
      let msg = err##message in
      if code = err##_TIMEOUT then Firebug.console##debug(msg)
    in
    geo##getCurrentPosition(Js.wrap_callback f_success, Js.wrap_callback f_error, options)



let addbutton div =
  let display_a_button marker =
    let button = marker.element in
    Dom.appendChild div button;
    button##style##left <- js ((string_of_int (int_of_float marker.mk_tx - 12))^"px");
    button##style##top <- js ((string_of_int (int_of_float marker.mk_ty - 25))^"px");
    setClass button "grey_button";
    button##onclick <- Html.handler
    (fun _ ->
      List.iter
      (fun x -> if x.element = button then
          (start_marker := Some x; setClass button "red_button";)
           else Dom.removeChild div x.element) (!markers1);
      markers1 := [];
      Js._true) in

  List.iter display_a_button (!markers1)

let addbutton2 div =
  let display_a_button marker =
    let button = marker.element in
    Dom.appendChild div button;
    button##style##left <- js ((string_of_int (int_of_float marker.mk_tx))^"px");
    button##style##top <- js ((string_of_int (int_of_float marker.mk_ty))^"px");
    setClass button "grey_button";
    button##onclick <- Html.handler
        (fun _ ->
          List.iter
          (fun x -> if x.element = button then
              (end_marker := Some x; setClass button "green_button";)
               else Dom.removeChild div x.element) (!markers2);
          markers2 := [];
          Js._true) in

  List.iter display_a_button (!markers2)

let coord_tup_to_markers tups =
  List.map (fun i ->
      {
        lat = fst i;
        lon = snd i;
        mk_tx = (snd i -. st.params.param_upleft_lon) /. st.wdpp;
        mk_ty = (st.params.param_upleft_lat -. fst i) /. st.hdpp;
        element = Html.createButton doc;
      }) tups


(* onload _ loads all the required HTML elements upon GUI launching *)
let onload _ =
  let start_icon = Html.createButton doc in
  (* start_icon##src <- js "start.png"; *)
  setClass start_icon "red_button";

  let end_icon = Html.createButton doc in
  (* end_icon##src <- js "dest.png"; *)
  setClass end_icon "green_button";
  (* Dom.appendChild div_map_container img_start; *)
  (* img_dest##style##visibility <- js "visible"; *)
  (* ==================== begin div map-container ==================== *)

  let div_map_container = Html.createDiv doc in
  setClass div_map_container "map-container";
  Dom.appendChild doc##body div_map_container;
  (* append_text div_map_container "Loading.."; *)






  (* let img_map = Html.createImg doc in
     setId img_map "map";
     img_map##src <- js "../tiles/1.png";
     Dom.appendChild div_mapbody img_map; *)
  let canvas_w = div_map_container##clientWidth in
  let canvas_h = div_map_container##clientHeight in
  st.params <- {st.params with width = float_of_int canvas_w;
                              height = float_of_int canvas_h};

  let coordinates = [(42.4417101,-76.4853911);(42.4405917,-76.4969232)] in
  let canvas = create_canvas canvas_w canvas_h in
  Dom.appendChild div_map_container canvas;
  let context = canvas##getContext (Html._2d_) in
  let offset = (5.0, 3.0) in
  (* draw_background canvas context draw_line (js "../tiles/1.png"); *)
  let _ = http_get_res st draw_background canvas context in
  (* let _ = http_get_res st.params st in *)

  (* let i = draw_background canvas context (js ("base_url"^"?index=5&path="^(!img_path))) offset in *)
  (* clear_background i canvas context; *)
  (* clear_background canvas context (js "../tiles/2.png"); *)
  (* let button = Dom_html.createButton ~_type:(Js.string "button") doc in
     Dom.appendChild div_map_container button;
     button##style##left <- js ((string_of_int 500)^"px");
     button##style##top <- js ((string_of_int 200)^"px");
     button##style##position <- js "absolute";
     button##style##zIndex <- js "2"; *)

  (* let img_map = Html.createImg doc in *)
  (* Dom.appendChild div_mapbody img_map; *)
  (* img_map##onload <- Html.handler
      (fun ev -> context##drawImage (img_map, (10.), (10.)); Js._false);
     setId img_map "map"; *)
  (* ==================== end div map-container ==================== *)







  let real_lrlat st =
    st.params.param_upleft_lat -. (st.hdpp *. st.params.height) in

  let real_lrlat st =
    st.params.param_upleft_lon +. (st.wdpp *. st.params.width) in

  let shift_left (delta:float) st =
    let new_params = {
      st.params with param_upleft_lon = st.params.param_upleft_lon -. delta;
                     param_lowright_lon = st.params.param_lowright_lon -. delta;
    } in
    st.params <- new_params in

  let shift_right (delta:float) st =
    let new_params = {
      st.params with param_upleft_lon = st.params.param_upleft_lon +. delta;
                     param_lowright_lon = st.params.param_lowright_lon +. delta;
    } in
    st.params <- new_params in

  let shift_up (delta:float) st =
    let new_params = {
      st.params with param_upleft_lat = st.params.param_upleft_lat +. delta;
                     param_lowright_lat = st.params.param_lowright_lat +. delta;
    } in
    st.params <- new_params in

  let shift_down (delta:float) st =
    let new_params = {
      st.params with param_upleft_lat = st.params.param_upleft_lat -. delta;
                     param_lowright_lat = st.params.param_lowright_lat -. delta;
    } in
    st.params <- new_params in

  let update_required st =
    st.params.param_upleft_lon < st.ullon_bound ||
    st.params.param_upleft_lat > st.ullat_bound ||
    st.params.param_lowright_lon > st.lrlon_bound ||
    st.params.param_lowright_lat < st.lrlat_bound in

(*   let remove_markers (marker_dom : Html.divElement Js.t) st =
    let _ = List.map (fun mk -> Dom.removeChild marker_dom mk.element) st.markers in
    st.markers <- [] in

  let update_markers st =
    let new_markers =
      List.map (fun mk ->
          {
            mk with mk_tx = (mk.lon -. st.params.param_upleft_lon) /. st.wdpp -.7. -. st.tx;
                    mk_ty = (st.params.param_upleft_lat -. mk.lat) /. st.hdpp -.7. -. st.ty;
          }
        ) st.markers in
    st.markers <- new_markers in *)






  (* let update_trans _ =
    draw_background canvas context src offset *)

  let on_drag dx dy st = ()  in


(*   let zoom direction st = 
    let w = st.params.width |> float_of_int in
    let h = st.params.height |> float_of_int in
    let ratio = w / h in
    let lvl = 2. ** (float_of_int st.current_depth) in
    let delta = direction *. delta_zoom / lvl in delta *)




  let zoom_in st =
    if st.current_depth = max_depth then () else

    let width = st.params.width in
    let height = st.params.height in

    let ullon = st.params.param_upleft_lon in
    let ullat = st.params.param_upleft_lat in
    let lrlon = st.params.param_upleft_lon +. st.wdpp *. width in
    let lrlat = st.params.param_upleft_lat -. st.hdpp *. height in

    let delta_lon = (lrlon -. ullon) /. 4. in
    let delta_lat = (ullat -. lrlat) /. 4. in
    let new_params = {
      param_upleft_lon    =   ullon +. delta_lon;
      param_upleft_lat    =   ullat -. delta_lat;
      param_lowright_lon  =   lrlon -. delta_lon;
      param_lowright_lat  =   lrlat +. delta_lat;
      width               =   width;
      height              =   height;
    } in
    st.params <- new_params;
    http_get_res st draw_background canvas context in


  let zoom_out st =
    if st.current_depth = min_depth then () else
    
    let width = st.params.width in
    let height = st.params.height in

    let ullon = st.params.param_upleft_lon in
    let ullat = st.params.param_upleft_lat in
    let lrlon = st.params.param_upleft_lon +. st.wdpp *. width in
    let lrlat = st.params.param_upleft_lat -. st.hdpp *. height in

    let delta_lon = (lrlon -. ullon) /. 2. in
    let delta_lat = (ullat -. lrlat) /. 2. in
    let new_lrlon = lrlon +. delta_lon |> string_of_float in
    let new_lrlat = lrlat -. delta_lat |> string_of_float in
    Dom_html.window##alert (js (new_lrlon ^ " " ^ new_lrlat));
    let new_params = {
      param_upleft_lon    =   ullon -. delta_lon;
      param_upleft_lat    =   ullat +. delta_lat;
      param_lowright_lon  =   lrlon +. delta_lon;
      param_lowright_lat  =   lrlat -. delta_lat;
      width               =   width;
      height              =   height;
    } in
    st.params <- new_params;
    http_get_res st draw_background canvas context in











  let div_markers = Html.createDiv doc in
  setId div_markers "markers";
  Dom.appendChild doc##body div_markers;

  let div_actions = Html.createDiv doc in
  setClass div_actions "actions";
  Dom.appendChild doc##body div_actions;

  (* let div_widget_card = Html.createDiv doc in
     setClass div_widget_card "widget card";
     Dom.appendChild div_actions div_widget_card; *)

  let div_card_content = Html.createDiv doc in
  setClass div_card_content "card-content";
  Dom.appendChild div_actions div_card_content;

  let div_autocomplete = Html.createDiv doc in
  setClass div_autocomplete "autocomplete";
  Dom.appendChild div_card_content div_autocomplete;

  let div_bottom = Html.createDiv doc in
  setClass div_bottom "bottom";
  Dom.appendChild div_autocomplete div_bottom;

  let input_1 = Html.createInput doc in
  setId input_1 "input1";
  input_1##placeholder <- js "Start Location";
  Dom.appendChild div_bottom input_1;

  let input_submit_1 =  Html.createInput doc in
  setId input_submit_1 "input1_submit";
  input_submit_1##setAttribute(js "type", js "submit");
  input_submit_1##setAttribute(js "value", js "Find");
  (* input_submit_1##value <- js "Find"; *)
  Dom.appendChild div_bottom input_submit_1;
  input_submit_1##onclick <- Html.handler
      (fun _ ->
        clear_start div_map_container;
        let n = (input_1##value |> Js.to_string) in
        http_get_nodes_by_name 1 n coord_tup_to_markers addbutton div_map_container;
      Js._true);

  let input_2 = Html.createInput doc in
  setId input_2 "input2";
  input_2##placeholder <- js "Destination";
  Dom.appendChild div_autocomplete input_2;


  let input_submit_2 =  Html.createInput doc in
  setId input_submit_2 "input2_submit";
  input_submit_2##setAttribute(js "type", js "submit");
  input_submit_2##setAttribute(js "value", js "Find");
  Dom.appendChild div_autocomplete input_submit_2;
  input_submit_2##onclick <- Html.handler
      (fun _ ->
        clear_end div_map_container;
        let n = (input_2##value |> Js.to_string) in
        http_get_nodes_by_name 2 n coord_tup_to_markers addbutton2 div_map_container;
      Js._true);




  div_map_container##ondblclick <- Html.handler
      (fun ev ->
         (* clear_goals div_map_container; *)
         (if (!start_marker <> None && !end_marker <> None)
          then
            (
            clear_all div_map_container;
            Dom_html.window##alert (js "1");
            input_1##value <- js "";
            input_2##value <- js "";
            (* the server needs to find the location name as well as the lat,lon,mk_tx, mk_ty *)
            Dom.appendChild div_map_container start_icon;
            start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
            start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
            start_marker := Some {
                lat = 0.0;
                lon = 0.0;
                mk_tx = 0.0;
                mk_ty = 0.0;
                element = start_icon;
              };
            ())
          else if (!start_marker = None && !end_marker = None)
          then
            (
            (* clear_all div_map_container; *)
            Dom_html.window##alert (js "2");

             Dom.appendChild div_map_container start_icon;
             start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             start_marker := Some {
                 lat = 0.0;
                 lon = 0.0;
                 mk_tx = 0.0;
                 mk_ty = 0.0;
                 element = start_icon;
               };
             ())
          else if (!start_marker <> None && !end_marker = None)
          then
            (Dom.appendChild div_map_container end_icon;
             end_icon##style##visibility <- js "visible";
             end_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             end_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             Dom_html.window##alert (js "3");
             end_marker := Some {
                 lat = 0.0;
                 lon = 0.0;
                 mk_tx = 0.0;
                 mk_ty = 0.0;
                 element = end_icon;
               };
             ())
          else if (!start_marker = None && !end_marker <> None)
          then
            (Dom.appendChild div_map_container start_icon;
            Dom_html.window##alert (js "4");
             start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             start_marker := Some {
                 lat = 0.0;
                 lon = 0.0;
                 mk_tx = 0.0;
                 mk_ty = 0.0;
                 element = start_icon;
               };
             ())
          else ());
         (* 把点去掉 把输入框换成新的 *)

         Js._true);
  (* let span_search_container = Html.createSpan doc in
     setClass span_search_container "search-container";
     Dom.appendChild div_card_content span_search_container;

     let label_for_tags = Html.createLabel doc in
     label_for_tags##htmlFor <- js "tags";
     Dom.appendChild span_search_container label_for_tags;

     let input_1 = Html.createInput doc in
     setClass input_1 "search";
     setId input_1 "tags";
     input_1##placeholder <- js "Search locations";
     Dom.appendChild span_search_container input_1; *)

  (* ==================== begin icons ==================== *)

  let span_icons_container = Html.createSpan doc in
  setClass span_icons_container "icons-container";
  Dom.appendChild div_card_content span_icons_container;
  let a_zoomin = Html.createA doc in
  setClass a_zoomin "zoomin";

  a_zoomin##onclick <- Dom_html.handler
      (fun _ ->
        zoom_in st;
         Js._true);

  let i_plus = Html.createI doc in
  setClass i_plus "action-icon fa fa-2x fa-search-plus";
  Dom.appendChild a_zoomin i_plus;


  let a_zoomout = Html.createA doc in
  setClass a_zoomout "zoomout";

  a_zoomout##onclick <- Dom_html.handler
      (fun _ ->
        zoom_out st;
         Js._true);

  let i_minus = Html.createI doc in
  setClass i_minus "action-icon fa fa-2x fa-search-minus";
  Dom.appendChild a_zoomout i_minus;


  let a_info = Html.createA doc in
  setClass a_info "info";

  let i_question = Html.createI doc in
  setClass i_question "action-icon fa fa-2x fa-question-circle";
  Dom.appendChild a_info i_question;

  let div_info_text = Html.createDiv doc in
  setClass div_info_text "info-text";
  append_text div_info_text "You can also use arrow keys and -/= to zoom, or use the mouse drag and scroll wheel.
  Double click to begin routing & double click again to end and show route.";
  Dom.appendChild a_info div_info_text;

  let div_info_subtext = Html.createDiv doc in
  setClass div_info_subtext "info-subtext";
  append_text div_info_subtext "( click this bubble to close )";
  Dom.appendChild div_info_text div_info_subtext;

  Dom.appendChild span_icons_container a_zoomin;
  Dom.appendChild span_icons_container a_zoomout;
  Dom.appendChild span_icons_container a_info;

  (* When the question mark icon is clicked, show the info subtext *)
  a_info##onclick <- Dom_html.handler
      (fun _ ->
         if div_info_text##style##display = Js.string "none" then
           (div_info_text##style##display <- Js.string "block";Js._true)
         else
           (div_info_text##style##display <- Js.string "none";Js._true));

  (* ==================== end icons ==================== *)

  let div_nothing = Html.createDiv doc in
  Dom.appendChild div_actions div_nothing;

  (* Clear the text in the textbox when "Clear Route" is clicked *)

  let a_go = Html.createA doc in
  setClass a_go "clear waves-effect btn";
  append_text a_go "go";
  a_go##onclick <- Html.handler
      (fun _ ->
         draw_background_with_line canvas context "../tiles/00.png" (4., 4.) coordinates;
         Js._true);
  Dom.appendChild div_nothing a_go;

  let a_drive = Html.createA doc in
  setClass a_drive "clear waves-effect btn";
  append_text a_drive "drive";
  a_drive##onclick <- Html.handler
      (fun _ ->
         input_1##value <- js "";
         Js._true);
  Dom.appendChild div_nothing a_drive;

  let a_clear = Html.createA doc in
  setClass a_clear "clear waves-effect btn";
  append_text a_clear "clear";
  a_clear##onclick <- Html.handler
      (fun _ ->
        input_1##value <- js "";
        input_2##value <- js "";
        (* clear_all div_map_container; *)
        Js._true);
  Dom.appendChild div_nothing a_clear;


  autocomplete input_1;
  autocomplete input_2;

  (* let mx = ref 0 in
     let my = ref 0 in *)
  let startx = ref 0 in
  let starty = ref 0 in
  let endx = ref 0 in
  let endy = ref 0 in
  canvas##onmousedown <- Dom_html.handler
      (fun ev ->
         (* mx := ev##clientX; my := ev##clientY; *)
         startx := ev##clientX; starty := ev##clientY;
         let c1 =
           Html.addEventListener Html.document Html.Event.mousemove
             (Html.handler
                (fun ev -> ();
                  (* let x = ev##clientX and y = ev##clientY in
                     let dx = x - !mx and dy = y - !my in
                     if dy != 0 then
                     debug "y";
                     Dom_html.window##alert (js ("y is "^string_of_int dy));
                     if dx != 0 then
                     debug "x";
                     Dom_html.window##alert (js ("x is "^string_of_int dx));
                     mx := x; my := y; *)
                  Js._true))
             Js._true
         in
         let c2 = ref Js.null in
         c2 := Js.some
             (Html.addEventListener Html.document Html.Event.mouseup
                (Dom_html.handler
                   (fun ev ->
                      endx := ev##clientX; endy := ev##clientY;
                      let dx = !endx- !startx and dy = !endy - !starty in
                      (* if (dy <> 0 || dx <> 0) then *)
                        (* st.tx <- st.tx +. float_of_int dx;
                        st.ty <- st.ty +. float_of_int dy; *)
                      let new_param = {
                        st.params with
                        param_upleft_lon = st.params.param_upleft_lon
                                           -. (st.wdpp *. float_of_int dx);
                        param_lowright_lon = st.params.param_lowright_lon
                                             -. (st.wdpp *. float_of_int dx);
                        param_upleft_lat = st.params.param_upleft_lat
                                           +. (st.hdpp *. float_of_int dy);
                        param_lowright_lat = st.params.param_lowright_lat
                                           +. (st.hdpp *. float_of_int dy)
                      } in
                      st.params <- new_param;
                      http_get_res st draw_background canvas context;
                        (* Dom_html.window##alert (js ("y is "^string_of_int dy)); *)

                        (* Dom_html.window##alert (js ("x is "^string_of_int dx)); *)
                      Html.removeEventListener c1;
                      Js.Opt.iter !c2 Html.removeEventListener;
                      Js._true))
                Js._true);
         Js._false);
  Js._false




(* Start to load the page *)
let () =
  Dom_html.window##onload <- Dom_html.handler onload
