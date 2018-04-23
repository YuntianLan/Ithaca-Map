open Js_of_ocaml
open Js_of_ocaml_lwt


let width = 600
let height = width

module Html = Dom_html
let js = Js.string
let doc = Html.document



(* let create_canvas () =
  let d = Html.window##.document in
  let c = Html.createCanvas d in
  c##.width := n * 2 * truncate w + 1;
  c##.height := n * 2 * truncate h + 1;
   c *)
let button_type = Js.string "button"
let button txt action =
 let b = Dom_html.createInput ~_type:button_type doc in
 b##.value := Js.string txt;
 b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
 b

let toggle_button txt1 txt2 action =
  let state = ref false in
  let txt1 = Js.string txt1 in
  let txt2 = Js.string txt2 in
  let b = Dom_html.createInput ~_type:button_type doc in
  b##.value := txt1;
  b##.onclick := Dom_html.handler
    (fun _ ->
       state := not !state;
       b##.value := if !state then txt2 else txt1;
       action !state;
       Js._true);
  b


let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##.width := w; c##.height := h; c

let start _ =
  let canvas = create_canvas width height in
  let canvas' = create_canvas width height in
  Dom.appendChild Html.document##.body canvas;
  (* Dom.appendChild Html.window##.document##.body c; *)
  let ctx = canvas##getContext Html._2d_ in
  let ctx' = canvas'##getContext Html._2d_ in
  let paused = ref false in
  let follow = ref false in
  let add = Dom.appendChild in
  let ctrl = Html.createDiv doc in
  ctrl##.className := Js.string "controls";
  let d = Html.createDiv doc in
  add d (doc##createTextNode (Js.string "Click and drag mouse to rotate."));
  add ctrl d;
  let form = Html.createDiv doc in
  let br () = Html.createBr doc in
  begin
    add form (toggle_button "Pause" "Resume" (fun p -> paused := p));
    add form (br ());
    add form (toggle_button "Follow rotation" "Fixed position"
                (fun f -> follow := f));
  end;
  Dom.appendChild ctrl form;
  Js._false




let _ =
Html.window##.onload := Html.handler start
