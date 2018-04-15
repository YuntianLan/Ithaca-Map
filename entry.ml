(* file: entry.ml *)

let enter_cb entry () =
  let text = entry#text in
  Printf.printf "Entry contents: %s\n" text;
  flush stdout

let toggle checkbutton f () = f checkbutton#active

let main () =
  (* Create a new window; set title and border width *)
  let window = GWindow.window ~title:"Entry"
      ~width:200 ~height:100 ~border_width:10 () in

  (* Set a handler for destroy event that immediately exits GTK. *)
  window#connect#destroy ~callback:GMain.Main.quit;

  let vbox = GPack.vbox ~packing:window#add () in

  let entry = GEdit.entry ~text:"hello" ~max_length:500 ~packing:vbox#add () in
  entry#connect#activate ~callback:(enter_cb entry);
  let tmp_pos = entry#text_length in
  entry#insert_text " world" tmp_pos;
  entry#select_region ~start:0 ~stop:entry#text_length;

  let hbox = GPack.hbox ~packing:vbox#add () in
  let check = GButton.check_button ~label:"Editable"
      ~active:true ~packing:hbox#add () in
  check#connect#toggled ~callback:(toggle check entry#set_editable);

  let check = GButton.check_button ~label:"Visible"
      ~active:true ~packing:hbox#add () in
  check#connect#toggled ~callback:(toggle check entry#set_visibility);

  let button = GButton.button ~stock:`CLOSE ~packing:vbox#add () in
  button#connect#clicked ~callback:window#destroy;

  button#misc#set_can_default true;
  button#misc#grab_default ();

  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main ()
