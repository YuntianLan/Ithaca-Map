open OUnit2
open Graph


(* Test get_node_by_coord *)
(* [get_node_by_coord lat lon t] takes in an coordinate
   * and a MapGraph and returns the node that is closest
   * to the coordinate. *)

let t = Map.init_graph "graph/full.json"
let test_get_node_by_coord = [
  "test1" >:: (fun _ -> assert_equal (42.375543, -76.415228) 
              (Map.get_node_by_coord 42.40 (-76.3) t |> Map.node_to_coord));

  "test2" >:: (fun _ -> assert_equal (42.375543, -76.415228) 
              (Map.get_node_by_coord 42.40 (-76.3) t |> Map.node_to_coord));

]



(* Test get_node_by_name *)

let test_get_node_by_name = []


(* Test find_path *)

let test_find_path = [
  "interactive_testing" >:: (fun _ -> assert_equal "interactive" "interactive");
  ]


let tests =
  "test suite for Trie" >::: List.flatten [
    test_get_node_by_coord;
    test_get_node_by_name;
    test_find_path;
  ]
let _ = run_test_tt_main tests
