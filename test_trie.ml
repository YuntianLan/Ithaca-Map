open OUnit2
open Trie

let e = Trie.empty
let strs = ["data"; "data"; "datastruct"; "structure"; "ithaca"; "map"; "yuntian"; "hanqing"; "xinqi"; "xinzhe"]
let empty_str = Trie.insert e "" ""
let test_empty = [
  "empty_mem" >:: (fun _ -> assert_equal (List.init (List.length strs) (fun i -> false))
                      (List.map (fun i -> Trie.memb e i) strs));

  "empty_find" >:: (fun _ -> assert_equal (List.init (List.length strs) (fun i -> None))
                       (List.map (fun i -> Trie.find e i) strs));

  "empty_begin" >:: (fun _ -> assert_equal (List.init (List.length strs) (fun i -> []))
                        (List.map (fun i -> Trie.begin_with e (fun j -> true) i) strs));

]

let data = Trie.insert e "data" "data"
let data2 = Trie.insert data "data" "data"
let datastruct = Trie.insert data2 "datastruct" "datastruct"

let unit_test = [
  "empty_str" >:: (fun _ -> assert_equal true (Trie.memb empty_str ""));
  "empty_str'" >:: (fun _ -> assert_equal false (Trie.memb empty_str "a"));

  "data" >:: (fun _ -> assert_equal true (Trie.memb data "data"));
  "data_insensitive" >:: (fun _ -> assert_equal true (Trie.memb data "DAtA"));
  "data_substr" >:: (fun _ -> assert_equal false (Trie.memb data "da"));
  "data_substr'" >:: (fun _ -> assert_equal false (Trie.memb data ""));
  "data_find" >:: (fun _ -> assert_equal (Some "data") (Trie.find data "data"));
  "data_find'" >:: (fun _ -> assert_equal None (Trie.find data "dat"));

  "no_dup" >:: (fun _ -> assert_equal data data2);

  "datastruct_data" >:: (fun _ -> assert_equal true (Trie.memb datastruct "DAtA"));
  "datastruct" >:: (fun _ -> assert_equal true (Trie.memb datastruct "DAtAStrUCT"));
  "beiginwith_DA" >:: (fun _ -> assert_equal ["datastruct";"data"] (Trie.begin_with datastruct (fun i -> true) "DA"));
  "beiginwith_dataS" >:: (fun _ -> assert_equal ["datastruct"] (Trie.begin_with datastruct (fun i -> true) "DAtaS"));
  "beiginwith_empty" >:: (fun _ -> assert_equal ["datastruct";"data"] (Trie.begin_with datastruct (fun i -> true) ""));
  "beiginwith_shit" >:: (fun _ -> assert_equal [] (Trie.begin_with datastruct (fun i -> true) "shit"));
  "beiginwith_predicate" >:: (fun _ -> assert_equal [] (Trie.begin_with datastruct (fun i -> false) "DA"));
  "beiginwith_predicate'" >:: (fun _ -> assert_equal ["datastruct"]
                                 (Trie.begin_with datastruct (fun i ->
                                      if String.length i > 5 then true
                                      else false) "DA"));
]

let strs_trie = List.fold_left (fun acc i -> Trie.insert acc i i) e strs

let stress_test = [
  "ithac" >:: (fun _ -> assert_equal false (Trie.memb strs_trie "ithac"));
  "ithaca" >:: (fun _ -> assert_equal true (Trie.memb strs_trie "ithacA"));
  "epsilon" >:: (fun _ -> assert_equal false (Trie.memb strs_trie ""));

  "beginwithemp" >:: (fun _ -> assert_equal ((strs |> List.length) - 1)
                         ((Trie.begin_with strs_trie (fun i -> true) "")|> List.length));
  "beginwithxin" >:: (fun _ -> assert_equal ["xinqi";"xinzhe"]
                         (Trie.begin_with strs_trie (fun i -> true) "Xin"));
]
let tests =
  "test suite for Trie" >::: List.flatten [
    test_empty;
    unit_test;
    stress_test;
  ]
let _ = run_test_tt_main tests
