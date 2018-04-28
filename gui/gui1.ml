open Js_of_ocaml
open Js_of_ocaml_lwt
open Js

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html
let js = Js.string
let doc = Html.document

let create_canvas w h =
  let c = Html.createCanvas doc in
  c##width <- w;
  c##height <- h;
  c

let draw_image_on_context context img_src coord =
  let img = Html.createImg doc in
  img##src <- img_src;
  context##drawImage ((img), (fst coord), (snd coord))

let texture = Js.string "texture.jpg"
let js = Js.string
(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail

let onload _ =
  (* let doc = Html.document in *)

  let div = Html.createDiv doc in
  let h2 = Html.createH2 doc in
  let canvas = create_canvas 300 400 in
  let img = Html.createImg doc in
  Dom.appendChild div img;
  img##src <- js "texture.jpg";
  (* let context = canvas##getContext (Html._2d_) in *)
  (* ctx'##drawImage texture (0.) (0.); *)
  (* draw_image_on_context context (js "texture.jpg") (3.0,4.0); *)


  (* let sbox1 = Dom_html.createTextarea doc in
  sbox1##value <- Js.string "";
  sbox1##id <- Js.string "console";
  sbox1##focus();
  sbox1##select(); *)

  (* let sbox1 = get_element_by_id "tags" in
     sbox1##textContent <- Js.some (Js.string "Change Content"); *)

  let sbox1 = Html.createInput doc in
  let sbox2 = Html.createInput doc in
  Dom.appendChild div sbox1;
  Dom.appendChild div sbox2;
  sbox1##className <- js "action-icon fa fa-2x fa-search-plus";
  sbox2##className <- js "action-icon fa fa-2x fa-search-plus";
  let button = Html.createButton ~_type:(Js.string "button") doc in
  h2##textContent <- Js.some (Js.string "Let AJAX change this text");
  button##textContent <- Js.some (Js.string "Change Content");
  (* let _ =
    let state = ref false in
    Lwt_js_events.clicks button
      (fun _ev _ ->
         state := not !state;
         if !state then
           Lwt.bind (XmlHttpRequest.get "ajax_info.txt")
             (fun {XmlHttpRequest.content; _} -> div##innerHTML <- Js.string content; Lwt.return ())
         else begin
           div##innerHTML <- Js.string "";
           Dom.appendChild div h2;
           Lwt.return()
         end)
     in *)
  let _ =
    Lwt_js_events.clicks button
      (fun _ev _  ->
        (* h2##textContent <- Js.some(Js.string "WOWWWW"); *)
          let s = sbox1##value in
          h2##textContent <- Js.some s;
       Lwt.return()) in
  (* button##onclick <- Dom_html *)
  Dom.appendChild div canvas;
  Dom.appendChild div h2;
  Dom.appendChild doc##body div;
  Dom.appendChild doc##body button;
  Js._false

  let () =
    Dom_html.window##onload <- Dom_html.handler onload
