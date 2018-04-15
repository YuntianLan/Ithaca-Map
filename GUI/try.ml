open Graphics
open Camlimages
open GMain
open GdkKeysyms
open GtkMisc

let _ = Graphics.open_graph " 800x800"

(* [get_img img] returns an image according to input file name. *)
(* let get_img img =
  Png.load img [] |> array_of_image |> make_image *)

let create_image image =
  Images.load image [] |> Graphic_image.of_image

let init_tile = create_image "tiles/1.png"
let delete_input = create_image "images/delete_input.png"
let textbox = create_image "images/input.png"
let ok_button = create_image "images/ok_button.png"
let welcome = create_image "images/welcome.png"
let zoom_in = create_image "images/zoom-in.png"
let zoom_out = create_image "images/zoom-out.png"
let textbox = create_image "images/textbox.png"


let click_button x y xl xr yb yt =
  x > xl && x < xr && y > yb && y < yt


let rec draw_input () x y acc =
  let status = wait_next_event [Key_pressed; Button_down] in
  if status.keypressed then begin
    let c = status.key in
    moveto x y;
    if (Char.escaped c) = "\\b" && (acc |> String.length) > 0 then begin
      let back = String.sub acc 0 ((acc |> String.length) - 1) in
      draw_image delete_input (current_x () - 24) (current_y () - 1);
      draw_input () (current_x () - 24) (current_y ()) back
    end
    else if (Char.escaped c) = "\\b" && (acc |> String.length) = 0 then begin
      draw_input () 310 340 ""
    end
    (* else if Char.escaped c = "\\r" then begin
      clear_graph ();
      acc
    end *)
  else begin
      draw_char c;
      draw_input () (current_x ()) (current_y ()) (acc^(Char.escaped c))
    end
  end
  (* else begin
    let click_x = status.mouse_x in
    let click_y = status.mouse_y in
    if click_button click_x click_y 800 938 335 400 then begin
      clear_graph ();
      acc
    end
    else draw_input () x y acc
     end *)

let writeText x y =

let draw_init () =
  draw_image welcome 100 600;
  draw_image init_tile 300 100;
  draw_image textbox 300 450;
  draw_image textbox 300 400;
  draw_image zoom_in 600 200;
  draw_image zoom_out 600 100;
