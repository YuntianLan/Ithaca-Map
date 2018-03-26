(* GUI.mli
	init:   1. Create search bar and button
			2. Find the image of the curent location
			3. Put a red pin to locate the user
			4. Create zoom in and zoom button
			5. Auto matching with search bar
 *)

module type GUI = sig
  (* type state =
    | Init
    | pattern ->  *)

  (* [init_window] initializes the whole user interface. *)
  val init_window : unit->unit

  (* [zoom in] choose a picture of larger scale to display when the zoom in
   * button is clicked. *)
  val zoom_in : unit->unit

  (* [zoom out] choose a picture of smaller scale to display when the zoom in
   * button is clicked. *)
  val zoom_out : unit->unit

  (* [show route] plots the route from start location to destination on the
   * picture of the map. *)
  val show_route : unit->unit

  (* [search] choose a picture of smaller scale to display when the zoom
   * in button is clicked. *)
  val search : unit->unit

  (* [showLocation] shows a node on the map that represents the user's current
   * location
   * in button is clicked. *)
  val showLoc : unit->unit

end
