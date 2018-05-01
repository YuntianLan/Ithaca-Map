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

  "test2" >:: (fun _ -> assert_equal (42.318172, -76.374253) 
              (Map.get_node_by_coord 40.0 (-70.0) t |> Map.node_to_coord));

  "test3" >:: (fun _ -> assert_equal (42.318172, -76.374253) 
              (Map.get_node_by_coord 30.0 (-60.0) t |> Map.node_to_coord)); 

  "test4" >:: (fun _ -> assert_equal (42.5043439, -76.3593372)
              (Map.get_node_by_coord 100.0 (30.0) t |> Map.node_to_coord)); 

  "test5" >:: (fun _ -> assert_equal (42.318172, -76.374253)
              (Map.get_node_by_coord  (-60.0) (30.0) t |> Map.node_to_coord)); 

  "test6" >:: (fun _ -> assert_equal (42.318172, -76.374253)
              (Map.get_node_by_coord  (-60.0) (-30.0) t |> Map.node_to_coord)); 

  "test7" >:: (fun _ -> assert_equal (42.318172, -76.374253)
              (Map.get_node_by_coord 41.0 (-70.0) t |> Map.node_to_coord)); 

  "test8" >:: (fun _ -> assert_equal (42.7875307, -76.7166654)
              (Map.get_node_by_coord 43.0 (-80.0) t |> Map.node_to_coord)); 
]


(* Test get_node_by_name *)

let test_get_node_by_name = [
  "test1" >:: (fun _ -> assert_equal (42.4451475, -76.481154)
              (match (Map.get_node_by_name "Gates Hall" t) with 
              | Some x -> Map.node_to_coord x | None -> (0.0,0.0)));

(*   "test2" >:: (fun _ -> assert_equal (42.4281548, -76.4691226)
              (match (Map.get_node_by_name "Hillside Alliance Church" t) with 
              | Some x -> Map.node_to_coord x | None -> (0.0,0.0))); *)

]


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
