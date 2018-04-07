(* open Lwt
open Cohttp
open Cohttp_lwt_unix *)

module type Server = sig
	
	module Tree : Image.ImageTree
	module Graph : Graph.MapGraph

	type tree = Tree.t
	type graph = Graph.t
	type t

	val init_server : tree -> graph -> t
	val accept_connection : t -> unit

end