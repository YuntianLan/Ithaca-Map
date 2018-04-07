
module type GUI = sig

  val init_window : unit -> unit
  val zoom_in : unit -> unit
  val zoom_out : unit -> unit
  val show_route : unit -> unit
  val search : unit -> unit
  val showLoc : unit -> unit

end


module type Trie = sig

  type t

  type value
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : value -> t -> t
  val member : value -> t -> bool
  val find : value -> t -> t
  val remove : value -> t -> t
  val to_list : t -> value list


end
