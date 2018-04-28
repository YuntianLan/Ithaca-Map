open Js_of_ocaml
open Js_of_ocaml_lwt
open Js

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html
let js = Js.string
let doc = Html.document

(* let Html.createImg doc = Html.createImg doc
let Html.createDiv doc = Html.createDiv doc
let createSpan = Html.createSpan doc
let createInput = Html.createInput doc
let createLabel = Html.createLabel doc
let createAttr = Html.createA doc *)

let setClass elt s = elt##className <- js s
let setId elt s = elt##id <- js s

let append_text e s = Dom.appendChild e (doc##createTextNode (js s))

(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail


let onload _ =
  (* let doc = Html.document in *)

  let div_map_container = Html.createDiv doc in
  setClass div_map_container "map-container";
  Dom.appendChild doc##body div_map_container;
  (* append_text div_map_container "Loading.."; *)

  let div_mapbody = Html.createDiv doc in
  setClass div_mapbody "mapbody";
  Dom.appendChild div_map_container div_mapbody;
  (* append_text div_mapbody "Hi"; *)

  let img_map = Html.createImg doc in
  setId img_map "map";
  img_map##src <- js "../tiles/1.png";
  Dom.appendChild div_mapbody img_map;

  let img_dest = Html.createImg doc in
  setId img_dest "dest";
  Dom.appendChild doc##body img_dest;
  let div_markers = Html.createDiv doc in
  setId div_markers "markers";
  Dom.appendChild doc##body div_markers;

  let div_actions = Html.createDiv doc in
  setClass div_actions "actions";
  Dom.appendChild doc##body div_actions;

  let div_widget_card = Html.createDiv doc in
  setClass div_widget_card "widget card";
  Dom.appendChild div_actions div_widget_card;

  let div_card_content = Html.createDiv doc in
  setClass div_card_content "card-content";
  Dom.appendChild div_widget_card div_card_content;

  let span_search_container = Html.createSpan doc in
  setClass span_search_container "search-container";
  Dom.appendChild div_card_content span_search_container;
  let label_for_tags = Html.createLabel doc in
  label_for_tags##htmlFor <- js "tags";
  Dom.appendChild span_search_container label_for_tags;
  let input_search = Html.createInput doc in
  setClass input_search "search";
  setId input_search "tags";
  input_search##placeholder <- js "Search locations";
  Dom.appendChild span_search_container input_search;


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


  let div_nothing = Html.createDiv doc in
  Dom.appendChild div_actions div_nothing;

  let a_clear = Html.createA doc in
  setClass a_clear "clear waves-effect btn";
  append_text a_clear "clear route";
  a_clear##onclick <- Html.handler
    (fun _ ->
       (* state := not !state;
       b##.value := if !state then txt2 else txt1;
          action !state; *)
        input_search##value <- js "";
       Js._true);
  Dom.appendChild div_nothing a_clear;

  (* let _ =
    Lwt_js_events.clicks button
      (fun _ev _  ->
        (* h2##textContent <- Js.some(Js.string "WOWWWW"); *)
          let s = sbox1##value in
          h2##textContent <- Js.some s;
       Lwt.return()) in *)


  (* let h2 = Html.createH2 doc in
  let img = Html.createImg doc in
  Dom.appendChild div img;
  img##src <- js "texture.jpg";

  let sbox1 = Html.createInput doc in
  let sbox2 = Html.createInput doc in
  Dom.appendChild div sbox1;
  Dom.appendChild div sbox2;
  sbox1##className <- js "action-icon fa fa-2x fa-search-plus";
  sbox2##className <- js "action-icon fa fa-2x fa-search-plus";
  let button = Html.createButton ~_type:(Js.string "button") doc in
  h2##textContent <- Js.some (Js.string "Let AJAX change this text");

  button##onclick <- Dom_html
  Dom.appendChild div h2;
  Dom.appendChild doc##body button; *)
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
