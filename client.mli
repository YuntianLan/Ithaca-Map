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
  val init_window : unit -> unit

  (* [zoom_in] choose a picture of larger scale to display when the zoom in
   * button is clicked. *)
  val zoom_in : unit -> unit

  (* [zoom_out] choose a picture of smaller scale to display when the zoom in
   * button is clicked. *)
  val zoom_out : unit -> unit

  (* [show_route] plots the route from start location to destination on the
   * picture of the map. *)
  val show_route : unit -> unit

  (* [search] displays a picture of the chosen location of appropriate scale. *)
  val search : unit -> unit

  (* [showLoc] shows a node on the map that represents the user's current
   * location when the button is clicked. *)
  val showLoc : unit -> unit

end


module type Trie = sig

  (* [t] is the type of Trie *)
  type t


  (* [value] is the type of values in the Trie*)
  type value

  (* [empty] is the empty Trie *)
  val empty : t

  (* [is_empty trie] is [true] iff [trie] is empty. *)
  val is_empty : t -> bool

  (* [size trie] is the number of valid values in [trie]*)
  val size : t -> int

  (* [insert v trie] is [trie] with [v] present *)
  val insert : value -> t -> t

  (* [member v trie] is [true] iff [v] is a valid value in [trie] *)
  val member : value -> t -> bool

  (* [find v trie] is a Trie such that all the children match the prefix [v]*)
  val find : value -> t -> t

  (* [remove v trie] is a Trie such that not children match the prefix [v]*)
  val remove : value -> t -> t

  (* [to_list trie] is a list containing all the valid values *)
  val to_list : t -> value list


end
