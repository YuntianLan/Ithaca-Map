open Js_of_ocaml
open Js_of_ocaml_lwt
open Js
open Lwt

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html

(* Type for image request *)
type params = {
  param_upleft_lon: float;
  param_upleft_lat: float;
  param_lowright_lon: float;
  param_lowright_lat: float;
  width: float;
  height: float;
}

(* Type for marker on the map *)
type marker = {
  lat : float;
  lon : float;
  mk_tx : float;
  mk_ty : float;
  element : Html.buttonElement Js.t;
}

(* Map state *)
type client_state = {
  mutable params: params;
  mutable current_depth: int;
  mutable wdpp : float;
  mutable hdpp : float;
  mutable tx : float;
  mutable ty : float;
  mutable img_w : float;
  mutable img_h : float;
  mutable ullon_bound : float;
  mutable ullat_bound : float;
  mutable lrlon_bound : float;
  mutable lrlat_bound : float;
}


let img_path = ref "unset"

let js = Js.string
let doc = Html.document
let base_url = "http://127.0.0.1:8000/"

(* [http_get url] sends a GET request with [url, returns a Lwt Deferred type *)
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

let root_upleft_lon = -76.5527
let root_upleft_lat = 42.4883
let root_lowright_lon = -76.4649
let root_lowright_lat = 42.4235
let init_lowright_lon = -76.5246061523437504
let init_lowright_lat = 42.4622962890625

let coordinates = [(42.464,-76.530);(42.470,-76.540);(42.472,-76.544)]

(* Initial wdpp and hdpp for level 3 *)
let iwdpp = 0.00004287109374999839
let ihdpp = 0.00003164062500000259

let wdpps = [
  0.0; (* Dummy, for index simplicity *)
  iwdpp *. 4.;    (* level = 1 *)
  iwdpp *. 2.;    (* level = 2 *)
  iwdpp *. 1.;    (* level = 3 *)
  iwdpp *. 0.5;   (* level = 4 *)
  iwdpp *. 0.25;  (* level = 5 *)
  iwdpp *. 0.125; (* level = 6 *)
]

let hdpps = [
  0.0; (* Dummy, for index simplicity *)
  ihdpp *. 4.;    (* level = 1 *)
  ihdpp *. 2.;    (* level = 2 *)
  ihdpp *. 1.;    (* level = 3 *)
  ihdpp *. 0.5;   (* level = 4 *)
  ihdpp *. 0.25;  (* level = 5 *)
  ihdpp *. 0.125; (* level = 6 *)
]

let markers1 : marker list ref = ref []
let markers2 : marker list ref = ref []
let sugg : marker list ref = ref []
let sugg_name : string list ref = ref []
let start_marker = ref None
let end_marker = ref None
let meter = ref ""
let route = ref []
let route_color = ref "#e60000"

let img_map = Html.createImg doc


(* Dummy mutable values *)
let by_coord = ref (0.,0.)
let by_name = ref [(0.,0.)]
let img_path = ref ""
let autocomp = ref [""]


(* Round a float to a string to a specific decimal places *)
let round precision (x:float) =
  let sx = string_of_float x in
  let lst = String.split_on_char '.' sx in
  let second = List.nth lst 1 in
  let trimmed =
    let l = String.length second in
    if l < precision then
      second ^ (String.make (precision - l) '0')
    else String.sub second 0 precision in
  (List.hd lst) ^ "." ^ trimmed

(* Initial parameter *)
let param = {
  param_upleft_lon = init_upleft_lon;
  param_upleft_lat = init_upleft_lat;
  param_lowright_lon = init_lowright_lon;
  param_lowright_lat = init_lowright_lat;
  width = 0.;
  height = 0.;
}

(* Initial client state *)
let st = {
  params = param;
  current_depth = 0;
  wdpp = 0.;
  hdpp = 0.;
  tx = 0.;
  ty = 0.;
  img_w = 0.;
  img_h  = 0.;
  ullon_bound = 0.;
  ullat_bound = 0.;
  lrlon_bound = 0.;
  lrlat_bound = 0.;
}




(* [http_get url] asynchronously returns the message from the given url *)
let http_get url =
  XmlHttpRequest.get url >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

(* [http_get_autocomp s] is the helper function that,
 * given the input string s, asynchronously gets the 
 * list of possible search results *)
let http_get_autocomp (s:string) =
  let url = base_url^"?index=6"^"&input="^s in
  let start () =
    http_get url >>= (fun res ->
        let params = String.split_on_char ';' res in
        autocomp := params;
        Lwt.return ()) in
  ignore(start ());
  !autocomp

(* [http_get_route d draw_line context coord_tup_to_markers]
 * is the helper function that,
 * given the input string s, asynchronously gets the 
 * distance/route and draws the route on the map *)
let http_get_route drive draw_line context coord_tup_to_markers =
  let _ = route_color := if drive = "true" then "#e60000" else "#0000ff" in
  let sopt, eopt = !start_marker, !end_marker in
    match sopt, eopt with
    | Some s, Some e ->
      let slat = s.lat |> string_of_float in
      let slon = s.lon |> string_of_float in
      let elat = e.lat |> string_of_float in
      let elon = e.lon |> string_of_float in
      let url = base_url^"?index=3&drive="^drive^
        "&slat=" ^ slat ^ "&slon=" ^ slon ^
        "&elat=" ^ elat ^ "&elon=" ^ elon in
      let start () =
        http_get url >>= (fun res ->
          if (String.get res 0) = 'E' then
            let _ = Dom_html.window##alert 
              (js "Could not find route between locations") in
            Lwt.return ()
          else
            let tplst = String.split_on_char ' ' res in
            let dist = List.hd tplst in
            let slstlst = tplst |> List.tl |> List.hd in
            let slst = String.split_on_char ';' slstlst in
            let s2tp = (fun s ->
              let l2 = String.split_on_char ',' s in
              let lat = l2 |> List.hd |> float_of_string in
              let lon = l2 |> List.tl |> List.hd |> float_of_string in
              (lat,lon)
            ) in
            let flst = List.map s2tp slst in
            let fdist = (try float_of_string dist with _ -> 0.0) in
            if (fdist = 0.) then
              let _ = Dom_html.window##alert
                (js "Could not find route between locations") in
              Lwt.return ()
            else
              let _ = (meter := dist) in
              route := (flst |> coord_tup_to_markers);
              draw_line context (flst |> coord_tup_to_markers);
              Dom_html.window##alert (js ("Estimated travel distance: "
                ^(dist|>float_of_string|>round 2)^" kilometers"));
              Lwt.return ()
        ) in
        ignore (start ())
    | _, _ ->
      Dom_html.window##alert (js "Please select a start and end point!")


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
       let x = cor.mk_tx in
       let y = cor.mk_ty in
       context##lineWidth <- (5.);
       context##strokeStyle <- (js (!route_color));
       context##beginPath ();
       context##moveTo (prevX,prevY);
       context##lineTo (x,y);
       context##stroke ();
       context##closePath ();
       (x,y)
    )
    ((List.hd lst).mk_tx, (List.hd lst).mk_ty) lst

let draw_background_with_line canvas context src offset lst =
  img_map##onload <- Html.handler
      (fun ev ->
         context##clearRect (0.0,0.0,(float_of_int canvas##width),
          (float_of_int canvas##height));
         context##drawImage_full (img_map, fst(offset), snd(offset),
          (float_of_int canvas##width), (float_of_int canvas##height),
          0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
         let _ = draw_line context lst in
         Js._false);
  setId img_map "map";
  img_map##src <- src


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
  img_map##src <- src

let clear_line canvas context =
  context##clearRect (0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
  context##drawImage_full (img_map, st.tx, st.ty,
  (float_of_int canvas##width), (float_of_int canvas##height),
  0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height))

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

let update_marker (x:marker) =
  {
    x with mk_tx = (x.lon -. st.params.param_upleft_lon) /. st.wdpp;
           mk_ty = (st.params.param_upleft_lat -. x.lat) /. st.hdpp;
  }

let clear_update_all_button div canvas context =
  markers1 := List.map (fun x -> Dom.removeChild div x.element;
                         update_marker x) (!markers1);
  markers2 := List.map (fun x -> Dom.removeChild div x.element;
                         update_marker x) (!markers2);
  sugg := List.map (fun x -> Dom.removeChild div x.element;
                     update_marker x) (!sugg);
  route := List.map (fun x -> update_marker x) (!route);
  (match !start_marker with
   | None -> ()
   | Some x ->
     Dom.removeChild div x.element;
     let new_marker = update_marker x in
  start_marker := Some new_marker);
  (match !end_marker with
   | None -> ()
   | Some x ->
   Dom.removeChild div x.element;
   let new_marker = update_marker x in
  end_marker := Some new_marker)

let clear_all div canvas context =
  List.iter (fun x -> Dom.removeChild div x.element) (!markers1);
  List.iter (fun x -> Dom.removeChild div x.element) (!markers2);
  List.iter (fun x -> Dom.removeChild div x.element) (!sugg);

  (match !start_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  (match !end_marker with
   | None -> ()
   | Some x -> Dom.removeChild div x.element);
  clear_line canvas context;
  markers1 := [];
  markers2 := [];
  sugg := [];
  sugg_name := [];
  route := [];
  meter := "";
  start_marker := None;
  end_marker := None



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

         currentFocus := -1;

         setId a (Js.to_string textbox##id^ "autocomplete-list");
         setClass a "autocomplete-items";
         (match Js.Opt.to_option textbox##parentNode with
          | None -> failwith "error"
          | Some x -> Dom.appendChild x a);

         for i = 0 to List.length lst - 1 do
           let word = List.nth lst i in
           if(String.(sub word 0 (length v) |> uppercase_ascii) = String.uppercase_ascii v)
           (* create a DIV element for each matching element: *)
           then let b = ref (Html.createDiv doc) in
             (* make the matching letters bold *)
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

let show_icons div =
  let helper id marker =
    let button = marker.element in
    Dom.appendChild div button;
    button##style##left <- js ((string_of_int (int_of_float marker.mk_tx))^"px");
    button##style##top <- js ((string_of_int (int_of_float marker.mk_ty))^"px");
    setClass button "tooltip";

    let tooltip_text = Html.createSpan doc in
    setClass tooltip_text "tooltiptext";
    let name = List.nth (!sugg_name) id in
    append_text tooltip_text name;
    Dom.appendChild button tooltip_text;
    button##onclick <- Html.handler
      (fun _ ->
         List.iter
           (fun x -> if x.element = button then
               (end_marker := Some x; setClass button "green_button";)
             else Dom.removeChild div x.element) (!sugg);
         sugg := [];
         Js._true)
  in

  List.iteri helper (!sugg)

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


let display_start_end div marker color =
  match !marker with
  | None -> ()
  | Some x ->
  let button = x.element in
  Dom.appendChild div button;
  button##style##left <- js ((string_of_int (int_of_float x.mk_tx - 12))^"px");
  button##style##top <- js ((string_of_int (int_of_float x.mk_ty - 25))^"px");
  setClass button color


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

(* ========= HTTP requests ========== *)


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

(* [split_coord_name_list s] is the list of coordinate tuples parsed from [s]
 * requries: [s] must be in the form "coord1,coord2,name1;coord3,coord4,name2;..."*)
let split_coord_name_list (s:string) : ((float*float)*string) list =
  let params = String.split_on_char ';' s in
  let tups = List.map (fun i ->
      let coords = String.split_on_char ',' i in
      let res_lat = List.nth coords 0 |> float_of_string in
      let res_lon = List.nth coords 1 |> float_of_string in
      let name = List.nth coords 2 in
      ((res_lat, res_lon), name)
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






let http_get_res st callback canvas context div_map_container =
  let url = base_url^"?index=4"^
            "&upleft_lat="^round 10 st.params.param_upleft_lat^
            "&upleft_lon="^round 10 st.params.param_upleft_lon^
            "&lowright_lat="^round 10 st.params.param_lowright_lat^
            "&lowright_lon="^round 10 st.params.param_lowright_lon^
            "&width="^round 10 st.params.width^
            "&height="^round 10 st.params.height in
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
        st.wdpp <- List.nth wdpps st.current_depth;
        st.hdpp <- List.nth hdpps st.current_depth;
        st.tx <- (st.params.param_upleft_lon -. st.ullon_bound) /. st.wdpp;
        st.ty <- ( st.ullat_bound -. st.params.param_upleft_lat) /. st.hdpp;
        clear_update_all_button div_map_container canvas context;
        addbutton div_map_container;
        addbutton2 div_map_container;
        show_icons div_map_container;
        display_start_end div_map_container start_marker "red_button";
        display_start_end div_map_container end_marker "green_button";
        if ((!route) |> List.length <= 0) then
          callback canvas context (js (base_url^"?index=5&path="^res)) (st.tx, st.ty)
        else
          draw_background_with_line canvas context (js 
            (base_url^"?index=5&path="^res)) (st.tx, st.ty) (!route);
        Lwt.return ()) in
  ignore(start ())


let http_get_nodes_by_type type_name div_map_container =
  let url = base_url^"?index=7"^"&type="^type_name in
  let start () =
    http_get url >>= (fun res ->
        List.iter (fun x -> Dom.removeChild div_map_container x.element) (!sugg);
        let coord_name = res |> split_coord_name_list in
        let coords = List.map (fun i -> fst i) coord_name in
        let names = List.map (fun i -> snd i) coord_name in
        sugg := coords |> coord_tup_to_markers;
        sugg_name := names;
        show_icons div_map_container;
        Lwt.return ()) in
  ignore(start ())

let pix2coord x y =
  let lon = st.params.param_upleft_lon +. x *. st.wdpp in
  let lat = st.params.param_upleft_lat -. y *. st.hdpp in
  (lon, lat)


(* ========= HTTP requests ========== *)
(* onload _ loads all the required HTML elements upon GUI launching *)
let onload _ =
  let start_icon = Html.createButton doc in
  setClass start_icon "red_button";

  let end_icon = Html.createButton doc in
  setClass end_icon "green_button";

  (* ==================== begin div map-container ==================== *)
  let div_map_container = Html.createDiv doc in
  setClass div_map_container "map-container";
  Dom.appendChild doc##body div_map_container;

  let canvas_w = div_map_container##clientWidth in
  let canvas_h = div_map_container##clientHeight in
  st.params <- {st.params with width = float_of_int canvas_w;
                              height = float_of_int canvas_h};

  let canvas = create_canvas canvas_w canvas_h in
  Dom.appendChild div_map_container canvas;
  let context = canvas##getContext (Html._2d_) in
  let _ = http_get_res st draw_background canvas context div_map_container in

  (* ==================== end div map-container ==================== *)

  let zoom_in st =
    if st.current_depth = max_depth then () else

    let width = st.params.width in
    let height = st.params.height in

    let ullon = st.params.param_upleft_lon in
    let ullat = st.params.param_upleft_lat in
    (* let lrlon = st.params.param_upleft_lon +. st.wdpp *. width in
    let lrlat = st.params.param_upleft_lat -. st.hdpp *. height in *)
    let lrlon = st.params.param_lowright_lon in
    let lrlat = st.params.param_lowright_lat in

    let delta_lon = (lrlon -. ullon) /. 2. in
    let delta_lat = (ullat -. lrlat) /. 2. in
    let new_params = {
      param_upleft_lon    =   ullon;
      param_upleft_lat    =   ullat;
      param_lowright_lon  =   lrlon -. delta_lon;
      param_lowright_lat  =   lrlat +. delta_lat;
      width               =   width;
      height              =   height;
    } in

    st.params <- new_params;
    http_get_res st draw_background canvas context div_map_container in


  let zoom_out st =
    if st.current_depth = min_depth then () else

    let width = st.params.width in
    let height = st.params.height in

    let ullon = st.params.param_upleft_lon in
    let ullat = st.params.param_upleft_lat in
    let lrlon = st.params.param_lowright_lon in
    let lrlat = st.params.param_lowright_lat in

    let delta_lon = (lrlon -. ullon) in
    let delta_lat = (ullat -. lrlat) in
    let new_params = {
      param_upleft_lon    =   ullon;
      param_upleft_lat    =   ullat;
      param_lowright_lon  =   lrlon +. delta_lon;
      param_lowright_lat  =   lrlat -. delta_lat;
      width               =   width;
      height              =   height;
    } in

    st.params <- new_params;
    http_get_res st draw_background canvas context div_map_container in


  let div_markers = Html.createDiv doc in
  setId div_markers "markers";
  Dom.appendChild doc##body div_markers;

  let div_actions = Html.createDiv doc in
  setClass div_actions "actions";
  Dom.appendChild doc##body div_actions;

  let div_icons = Html.createDiv doc in
  setClass div_icons "icons";
  Dom.appendChild doc##body div_icons;

  let lib_button = Html.createA doc in
  Dom.appendChild div_icons lib_button;
  setClass lib_button "category_icon";

  let lib_icon = Html.createImg doc in
  lib_icon##src <- js "static/library.png";
  Dom.appendChild lib_button lib_icon;

  lib_button##onclick <- Html.handler
      (fun _ ->
         http_get_nodes_by_type "study" div_map_container;
      Js._true);

  let shop_button = Html.createA doc in
  Dom.appendChild div_icons shop_button;
  setClass shop_button "category_icon";

  let shop_icon = Html.createImg doc in
  shop_icon##src <- js "static/shop.png";
  Dom.appendChild shop_button shop_icon;

  shop_button##onclick <- Html.handler
      (fun _ ->
         http_get_nodes_by_type "shop" div_map_container;
         Js._true);

  let food_button = Html.createA doc in
  Dom.appendChild div_icons food_button;
  setClass food_button "category_icon";


  let food_icon = Html.createImg doc in
  food_icon##src <- js "static/food.png";
  Dom.appendChild food_button food_icon;

  food_button##onclick <- Html.handler
      (fun _ ->
         http_get_nodes_by_type "fooddrink" div_map_container;
         Js._true);

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
            clear_all div_map_container canvas context;
            input_1##value <- js "";
            input_2##value <- js "";
            (* the server needs to find the location name as well as the lat,lon,mk_tx, mk_ty *)
            Dom.appendChild div_map_container start_icon;
            start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
            start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
            let x = float_of_int ev##clientX in
            let y = float_of_int ev##clientY in
            let (longi, lati) = pix2coord x y in
            start_marker := Some {
                lat = lati;
                lon = longi;
                mk_tx = x;
                mk_ty = y;
                element = start_icon;
              };
            ())
          else if (!start_marker = None && !end_marker = None)
          then
            (
             Dom.appendChild div_map_container start_icon;
             start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             let x = float_of_int ev##clientX in
             let y = float_of_int ev##clientY in
             let (longi, lati) = pix2coord x y in
             start_marker := Some {
                 lat = lati;
                 lon = longi;
                 mk_tx = x;
                 mk_ty = y;
                 element = start_icon;
               };
             ())
          else if (!start_marker <> None && !end_marker = None)
          then
            (Dom.appendChild div_map_container end_icon;
             end_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             end_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             let x = float_of_int ev##clientX in
             let y = float_of_int ev##clientY in
             let (longi, lati) = pix2coord x y in
             end_marker := Some {
                 lat = lati;
                 lon = longi;
                 mk_tx = x;
                 mk_ty = y;
                 element = end_icon;
               };
             ())
          else if (!start_marker = None && !end_marker <> None)
          then
            (Dom.appendChild div_map_container start_icon;
             start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
             start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
             let x = float_of_int ev##clientX in
             let y = float_of_int ev##clientY in
             let (longi, lati) = pix2coord x y in
             start_marker := Some {
                 lat = lati;
                 lon = longi;
                 mk_tx = x;
                 mk_ty = y;
                 element = start_icon;
               };
             ())
          else ());
         (* Remove the point and replace the input box with a new one *)
         Js._true);

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
  append_text div_info_text "You can also use arrow keys and -/= to zoom, 
  or use the mouse drag and scroll wheel.
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
  append_text a_go "walk";
  a_go##onclick <- Html.handler
      (fun _ ->
         clear_line canvas context;
        http_get_route "false" draw_line context coord_tup_to_markers;
         Js._true);
  Dom.appendChild div_nothing a_go;

  let a_drive = Html.createA doc in
  setClass a_drive "clear waves-effect btn";
  append_text a_drive "drive";
  a_drive##onclick <- Html.handler
      (fun _ ->
         clear_line canvas context;
         http_get_route "true" draw_line context coord_tup_to_markers;
         Js._true);
  Dom.appendChild div_nothing a_drive;

  let a_clear = Html.createA doc in
  setClass a_clear "clear waves-effect btn";
  append_text a_clear "clear";
  a_clear##onclick <- Html.handler
      (fun _ ->
        input_1##value <- js "";
        input_2##value <- js "";
        clear_all div_map_container canvas context;
        Js._true);
  Dom.appendChild div_nothing a_clear;

  autocomplete input_1;
  autocomplete input_2;

  let startx = ref 0 in
  let starty = ref 0 in
  let endx = ref 0 in
  let endy = ref 0 in
  canvas##onmousedown <- Dom_html.handler
      (fun ev ->
         startx := ev##clientX; starty := ev##clientY;
         let c1 =
           Html.addEventListener Html.document Html.Event.mousemove
             (Html.handler
                (fun ev -> ();
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
                      if (dy <> 0 || dx <> 0) then
                        let ullon = st.params.param_upleft_lon in
                        let ullat = st.params.param_upleft_lat in
                        let dx = if ((ullon -. (st.wdpp *. (float_of_int dx)))
                            < root_upleft_lon) then
                          (ullon -. root_upleft_lon) /. st.wdpp |> int_of_float else dx in
                        let dy = if ((ullat +. (st.hdpp *. (float_of_int dy)))
                            > root_upleft_lat) then
                          (root_upleft_lat -. ullat) /. st.hdpp |> int_of_float else dy in

                        let new_param = {
                          st.params with
                          param_upleft_lon = max root_upleft_lon (st.params.param_upleft_lon
                                                                  -. (st.wdpp *. float_of_int dx));
                          param_lowright_lon = st.params.param_lowright_lon
                                               -. (st.wdpp *. float_of_int dx);
                          param_upleft_lat = min root_upleft_lat (st.params.param_upleft_lat
                                                                  +. (st.hdpp *. float_of_int dy));
                          param_lowright_lat = st.params.param_lowright_lat
                                             +. (st.hdpp *. float_of_int dy)
                        } in
                        
                        st.params <- new_param;
                        http_get_res st draw_background canvas context div_map_container;
                      else
                        ();

                      Html.removeEventListener c1;
                      Js.Opt.iter !c2 Html.removeEventListener;
                      Js._true))
                Js._true);
         Js._false);
  Js._false



(* Start to load the page *)
let () =
  Dom_html.window##onload <- Dom_html.handler onload
