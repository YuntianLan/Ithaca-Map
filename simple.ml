open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let enter_cb entry () =
  let text = entry#text in
  Printf.printf "Entry contents: %s\n" text;
  flush stdout

(* Create a Buttn Box with the specified parameters *)
let create_bbox direction title spacing child_width child_height layout =
  let frame = GBin.frame ~label:title () in
  let bbox = GPack.button_box direction ~border_width:5 ~layout
      ~child_height ~child_width ~spacing ~height:50 ~packing:frame#add () in


  let button1 = GButton.button ~label:"Zoom In"
      ~packing:bbox#add () in
  button1#connect#clicked ~callback: (fun () -> prerr_endline "zoom_in");

  let button2 = GButton.button ~label:"Zoom Out"
      ~packing:bbox#add () in
  button2#connect#clicked ~callback: (fun () -> prerr_endline "zoom_out");
  (* GButton.button ~stock:`HELP ~packing:bbox#add (); *)


  frame#coerce


let main () =
  let window = GWindow.window ~width:800 ~height:700
      ~title:"Simple lablgtk program" () in
  (* let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit; *)

  let vbox = GPack.vbox ~packing:window#add () in


  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
     let factory = new GMenu.factory menubar in
     let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

  let frame = GBin.frame ~packing:vbox#pack () in
  GMisc.label ~text:"This is Ithaca-Map Project" ~height:60 ~packing:frame#add ();

  let destination = GEdit.entry ~text:"" ~max_length:500 ~packing:vbox#pack () in
  destination#connect#activate ~callback:(enter_cb destination);
  (* let tmp_pos = destination#text_length in
  destination#insert_text " world" tmp_pos;
  destination#select_region ~start:0 ~stop:destination#text_length; *)




  (* Button *)
  (* let button1 = GButton.button ~label:"Zoom In"
      ~packing:vbox#add () in
  button1#connect#clicked ~callback: (fun () -> prerr_endline "zoom_in");

  let button2 = GButton.button ~label:"Zoom Out"
      ~packing:vbox#add () in
  button2#connect#clicked ~callback: (fun () -> prerr_endline "zoom_out"); *)

  (* let frame_vert = GBin.frame ~label:"Vertical Button Boxes"
  ~packing:(vbox#pack ~expand:true ~fill:true ~padding:10) () in *)

(* Create the button box *)
  let hbox = GPack.hbox ~border_width:10 ~width:50 ~height:120 ~packing:vbox#pack () in
  (* hbox#add (create_bbox `VERTICAL "Spread (spacing 5)" 5 85 20 `SPREAD); *)
  hbox#pack (create_bbox `VERTICAL "View Map" 10 30 10 `SPREAD)
    ~expand:false ~fill:false ~padding:5;




  (* let img = GMisc.image ~pixbuf () in img *)



  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
