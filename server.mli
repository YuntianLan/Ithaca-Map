(* A [Server] sits on the cloud VM and responds to client
 * requests by looking at its stored tree and graph.
 * The tree and graph are pre-processed immutable data types
 * that provide API for the server to respond to the client.
 *)



(* [Tree] is a module representing the directories
 * of all the images with different resolutions. *)
module ImageTree : Image.MapImage

(* [Graph] is a module representing the relations 
 * between nodes on the map. *)
module MapGraph : Graph.MapGraph


(* [tree] is the type of MapTree
 * and is a synonym for [Tree.t]. *)
type tree = ImageTree.t

(* [graph] is the type of MapGraph
 * and is a synonym for [Tree.t]. *)
type graph = MapGraph.t

(* [t] is the type of server *)
type t

(* [init_server] builds the pre-processed tree
 * and graph and creates the server. *)
val init_server : unit -> t

(* [begin_service t] makes the server to start
 * listening to client requests continuously
 * until interrupted. *)
val begin_service : t -> unit







