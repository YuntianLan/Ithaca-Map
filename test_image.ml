open OUnit2
open Image

module M = Image.Images
let params_invalid = {
  M.param_upleft_lon = -5.0;
  param_upleft_lat = 2.;
  param_lowright_lon = -2.0;
  param_lowright_lat = 5.;
  width = 512.;
  height = 512.;
}
let params_512 = {
    M.param_upleft_lon = -76.5527;
    param_upleft_lat = 42.4883;
    param_lowright_lon = -76.4649;
    param_lowright_lat = 42.4235;
    width = 512.;
    height = 512.;
  }
let params_1200 = {
  M.param_upleft_lon = -76.517;
  param_upleft_lat = 42.4783;
  param_lowright_lon = -76.4749;
  param_lowright_lat = 42.4335;
  width = 1200.;
  height = 800.;
}

let init_image = M.init ()
let q1 = M.query_image init_image params_invalid
let q2 = M.query_image init_image params_512
let q3 = M.query_image init_image params_1200
let test = [
  "invalid_coord" >:: (fun _ -> assert_equal false q1.status);
  "valid" >:: (fun _ -> assert_equal true q2.status);
  "check_depth" >:: (fun _ -> assert_equal 1 q2.tree_depth);
  "check_grid" >:: (fun _ -> assert_equal [["tiles/0.png"; "tiles/1.png"]; ["tiles/2.png"; "tiles/3.png"]]
                       q2.img_grid);
  "ullon" >:: (fun _ -> assert_equal (-76.5527) q2.res_upleft_lon);
  "ullat" >:: (fun _ -> assert_equal (42.4883) q2.res_upleft_lat);
  "lwlon" >:: (fun _ -> assert_equal (-76.4649) q2.res_lowright_lon);
  "lwlat" >:: (fun _ -> assert_equal (42.4235) q2.res_lowright_lat);

  "valid_smaller" >:: (fun _ -> assert_equal true q3.status);
  "check_depth" >:: (fun _ -> assert_equal 4 q3.tree_depth);
  "ullon_in_root" >:: (fun _ -> assert_equal true (-76.5527 <= q2.res_upleft_lon));
  "ullon_contain" >:: (fun _ -> assert_equal true (params_1200.param_upleft_lon >= q2.res_upleft_lon));
  "ullat_in_root" >:: (fun _ -> assert_equal true (42.4883 >= q2.res_upleft_lat));
  "ullat_contain" >:: (fun _ -> assert_equal true (params_1200.param_upleft_lat <= q2.res_upleft_lat));
  "lwlon_in_root" >:: (fun _ -> assert_equal true (-76.4649 >= q2.res_lowright_lon));
  "lwlon_contain" >:: (fun _ -> assert_equal true (params_1200.param_lowright_lon <= q2.res_lowright_lon));
  "lwlat_in_root" >:: (fun _ -> assert_equal true (42.4235 <= q2.res_lowright_lat));
  "lwlat_contain" >:: (fun _ -> assert_equal true (params_1200.param_lowright_lat >= q2.res_lowright_lat));
]


let tests =
  "test suite for Image" >::: List.flatten [
    test;
  ]
let _ = run_test_tt_main tests
