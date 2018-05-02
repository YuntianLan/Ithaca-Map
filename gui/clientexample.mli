(* Example for how the client interacts with the server,
 * current supported actions:
 * 1. Nearest node coord given coord
 * 2. Nearest node coord given location name
 * 3. Path from one coord to another coord
 * 4. Result given a query parameter
 *
 *
 *
 *
 *)

val init_client : unit -> Unix.file_descr

val decode_service4 : unit -> string
(* return image path for now! *)
