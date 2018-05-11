open Js_of_ocaml
open Js_of_ocaml_lwt
open Js
open Clientgui

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html
let js = Js.string
let doc = Html.document

(* ========= constants ========== *)
let max_depth = 6
let min_depth = 1
let delta_zoom = 0.04
let delta_base_move = 0.03
let init_upleft_lon = -76.5527
let init_upleft_lat = 42.4883
let init_lowright_lon = -76.4649
let init_lowright_lat = 42.4235

let buttonlist = ref [(100,200); (350,400); (150,500)]
let buttondisplay = ref []
let buttontuple = ref Js.null
let buttonlist2 = ref [(400,200); (650,400); (450,500)]
let buttondisplay2 = ref []
let buttontuple2 = ref Js.null
let dbstart = ref None
let dbend = ref None

let coordinates = [(12.0,14.0);(30.0,40.0);(60.0,8.0);(388.0,200.0)]

let clear_goals div =
  List.iter (fun x -> Dom.removeChild div x) (!buttondisplay);
  List.iter (fun x -> Dom.removeChild div x) (!buttondisplay2);
  (match Js.Opt.to_option !buttontuple with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  (match Js.Opt.to_option !buttontuple2 with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  buttonlist := [];
  buttonlist2 := [];
  buttondisplay := [];
  buttondisplay2 := [];
  buttontuple := Js.null;
  buttontuple2 := Js.null

let clear_all div =
  List.iter (fun x -> Dom.removeChild div x) (!buttondisplay);
  List.iter (fun x -> Dom.removeChild div x) (!buttondisplay2);
  (match Js.Opt.to_option !buttontuple with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  (match Js.Opt.to_option !buttontuple2 with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  (match !dbstart with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  (match !dbend with
    | None -> ()
    | Some x -> Dom.removeChild div (x));
  buttonlist := [];
  buttonlist2 := [];
  buttondisplay := [];
  buttondisplay2 := [];
  buttontuple := Js.null;
  buttontuple2 := Js.null;
  dbstart := None;
  dbend := None



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


let draw_background canvas context onload src offset lst =
  let img_map = Html.createImg doc in
  img_map##onload <- Html.handler
      (fun ev ->
        context##clearRect (0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
        context##drawImage_full (img_map, fst(offset), snd(offset), (float_of_int canvas##width), (float_of_int canvas##height),0.0,0.0,(float_of_int canvas##width),(float_of_int canvas##height));
        onload context lst;
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
         doc##onclick <- Html.handler (fun ev -> (closeAllList (Js.Opt.to_option ev##target) (Dom_html.CoerceTo.element textbox));Js._true);
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
    let display_a_button (a,b) =
      let button = Dom_html.createButton ~_type:(Js.string "button") doc in
      Dom.appendChild div button;
      buttondisplay := button::(!buttondisplay);
      button##style##left <- js ((string_of_int a)^"px");
      button##style##top <- js ((string_of_int b)^"px");
      button##style##position <- js "absolute";
      button##style##zIndex <- js "2";
      button##onclick <- Html.handler
          (fun _ ->
          List.iter (fun x -> Dom.removeChild div x) (!buttondisplay);
          let button = Dom_html.createButton ~_type:(Js.string "button") doc in
          Dom.appendChild div button;
          buttondisplay := [];
          buttontuple := Js.some (button);
          button##style##left <- js ((string_of_int a)^"px");
          button##style##top <- js ((string_of_int b)^"px");
          button##style##position <- js "absolute";
          button##style##zIndex <- js "2";
          button##style##background <- js "red";
          (* Dom_html.window##alert (js (string_of_int a)); *)
          Js._true) in

    List.iter display_a_button (!buttonlist)

    let addbutton2 div =
    let display_a_button (a,b) =
      let button = Dom_html.createButton ~_type:(Js.string "button") doc in
      Dom.appendChild div button;
      buttondisplay2 := button::(!buttondisplay2);
      button##style##left <- js ((string_of_int a)^"px");
      button##style##top <- js ((string_of_int b)^"px");
      button##style##position <- js "absolute";
      button##style##zIndex <- js "2";
      button##onclick <- Html.handler
          (fun _ ->
          List.iter (fun x -> Dom.removeChild div x) (!buttondisplay2);
          let button = Dom_html.createButton ~_type:(Js.string "button") doc in
          Dom.appendChild div button;
          buttondisplay2 := [];
          buttontuple2 := Js.some (button);
          button##style##left <- js ((string_of_int a)^"px");
          button##style##top <- js ((string_of_int b)^"px");
          button##style##position <- js "absolute";
          button##style##zIndex <- js "2";
          button##style##background <- js "green";
          (* Dom_html.window##alert (js (string_of_int a)); *)
          Js._true) in

    List.iter display_a_button (!buttonlist2)

(* onload _ loads all the required HTML elements upon GUI launching *)
let onload _ =
  let start_icon = Html.createImg doc in
  start_icon##src <- js "start.png";
  setClass start_icon "icon";

  let end_icon = Html.createImg doc in
  end_icon##src <- js "dest.png";
  setClass end_icon "icon";
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
  let coordinates = [(12.0,14.0);(30.0,40.0);(60.0,8.0);(388.0,200.0)] in
  let canvas = create_canvas canvas_w canvas_h in
  Dom.appendChild div_map_container canvas;
  let context = canvas##getContext (Html._2d_) in
  let offset = (5.0, 3.0) in
  (* draw_background canvas context draw_line (js "../tiles/1.png"); *)
  let i = draw_background canvas context draw_line (js "../tiles/2.png") offset coordinates in
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
        (match Js.Opt.to_option !buttontuple with
          | None -> ()
          | Some x -> Dom.removeChild div_map_container (x);
                      buttontuple := Js.null);
        addbutton div_map_container;
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
        (match Js.Opt.to_option !buttontuple2 with
          | None -> ()
          | Some x -> Dom.removeChild div_map_container (x);
                      buttontuple2 := Js.null);
        addbutton2 div_map_container;
        Js._true);




div_map_container##ondblclick <- Html.handler
    (fun ev ->
      clear_goals div_map_container;
      (if (!dbstart <> None && !dbend <> None)
      then
        (clear_all div_map_container;
        Dom_html.window##alert (js "start");
        input_1##value <- js "";
        input_2##value <- js "";
        Dom.appendChild div_map_container start_icon;
        start_icon##style##visibility <- js "visible";
        start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
        start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
        dbstart := Some start_icon;
        ())
      else if (!dbstart = None && !dbend = None)
      then
        (clear_all div_map_container;
        Dom_html.window##alert (js "start");
        input_1##value <- js "";
        input_2##value <- js "";
        Dom.appendChild div_map_container start_icon;
        start_icon##style##visibility <- js "visible";
        start_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
        start_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
        dbstart := Some start_icon;
        ())
      else if (!dbstart <> None && !dbend = None)
      then
        (Dom.appendChild div_map_container end_icon;
        end_icon##style##visibility <- js "visible";
        end_icon##style##left <- js ((string_of_int (ev##clientX-12))^"px");
        end_icon##style##top <- js ((string_of_int (ev##clientY-25))^"px");
        Dom_html.window##alert (js "end");
        dbend := Some end_icon;
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
  let i_plus = Html.createI doc in
  setClass i_plus "action-icon fa fa-2x fa-search-plus";
  Dom.appendChild a_zoomin i_plus;


  let a_zoomout = Html.createA doc in
  setClass a_zoomout "zoomout";
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
         input_1##value <- js "";
         Js._true);
  Dom.appendChild div_nothing a_go;

  let a_clear = Html.createA doc in
  setClass a_clear "clear waves-effect btn";
  append_text a_clear "clear route";
  a_clear##onclick <- Html.handler
      (fun _ ->
        input_1##value <- js "";
        input_2##value <- js "";
        (match Js.Opt.to_option !buttontuple with
          | None -> ()
          | Some x -> Dom.removeChild div_map_container (x));
        (match Js.Opt.to_option !buttontuple2 with
          | None -> ()
          | Some x -> Dom.removeChild div_map_container (x));
        buttontuple := Js.null;
        buttontuple2 := Js.null;
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
                 if dy != 0 then
                   (* debug_msg (Format.sprintf "Mouse up y %d" dy); *)
                   (* Dom_html.window##alert (js ("y is "^string_of_int dy)); *)
                 if dx != 0 then
                   (* debug_msg (Format.sprintf "Mouse up x %d" dx); *)
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
