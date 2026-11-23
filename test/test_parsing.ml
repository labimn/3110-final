open OUnit2
module S = Spendwise.Savings

(* If Error there is a failure *)
let ok = function
  | Ok value -> ()
  | Error message -> assert_failure message

let make_goal ?(id = 1) ?(name = "A") ?(target = 500.) ?(deadline = None) () =
  (S.create_goal ~id ~name ~target ~deadline)

let test_ok _ = 
  ok (S.create_goal ~id:1 ~name:"A" ~target:500. ~deadline:(Some "2025-12-31"));
  ok (S.create_goal ~id:2 ~name:"B" ~target:1000. ~deadline:None);
  ok (S.create_goal ~id:3 ~name:"C" ~target:1. ~deadline:(Some "2024-01-01"));
  ok (S.create_goal ~id:4 ~name:"D" ~target:100. ~deadline:(Some "2023-06-30"))

let assert_money expected actual =
  assert_bool
    (Printf.sprintf "expected %.2f, got %.2f" expected actual)
    (abs_float (expected -. actual) < 0.000001)

let assert_goal_equal expected actual =
  assert_equal (S.id expected) (S.id actual);
  assert_equal (S.name expected) (S.name actual);
  assert_money (S.target expected) (S.target actual);
  assert_equal (S.deadline expected) (S.deadline actual);
  assert_equal (S.contributions expected) (S.contributions actual)

let test_round_trip goal =
  let first = goal in
  let second = Result.get_ok (S.create_goal ~id:(S.id first) ~name:(S.name first) ~target:(S.target first) ~deadline:(S.deadline first)) in
  let text = S.string_of_goals [second] in
  let second = Result.get_ok (S.load_goal text) in
  assert_goal_equal first second

let test_string_of_goals_round_trip _ =
  let goal = Result.get_ok (S.create_goal ~id:1 ~name:"A" ~target:500. ~deadline:(Some "2025-12-31")) in
  test_round_trip goal;
  let goal = Result.get_ok (S.create_goal ~id:2 ~name:"B" ~target:1000. ~deadline:None) in
  test_round_trip goal;
  let goal = Result.get_ok (S.create_goal ~id:3 ~name:"C" ~target:1. ~deadline:(Some "2024-01-01")) in
  test_round_trip goal;
  let goal = Result.get_ok (S.create_goal ~id:4 ~name:"D" ~target:100. ~deadline:(Some "2023-06-30")) in
  test_round_trip goal

let test_string_of_empty_list _ =
  assert_equal "" (S.string_of_goals [])

let test_load_goal_fields _ =
  let goal =
    Result.get_ok (S.load_goal "7,A,1200,2028-06-15,1234;250")
  in
  assert_equal 7 (S.id goal);
  assert_equal "A" (S.name goal);
  assert_money 1200. (S.target goal);
  assert_equal (Some "2028-06-15") (S.deadline goal);
  assert_equal [ 12.34; 2.5 ] (S.contributions goal);
  let goal =
    Result.get_ok (S.load_goal "8,B,500,,")
  in
  assert_equal 8 (S.id goal);
  assert_equal "B" (S.name goal);
  assert_money 500. (S.target goal);
  assert_equal None (S.deadline goal);
  assert_equal [] (S.contributions goal);
  let goal = Result.get_ok (S.load_goal "9,C,1,2024-01-01,") in
  assert_equal 9 (S.id goal);
  assert_equal "C" (S.name goal);
  assert_money 1. (S.target goal);
  assert_equal (Some "2024-01-01") (S.deadline goal);
  assert_equal [] (S.contributions goal);
  let goal = Result.get_ok (S.load_goal "10,D,100,2023-06-30,50;25;25") in
  assert_equal 10 (S.id goal);
  assert_equal "D" (S.name goal);
  assert_money 100. (S.target goal);
  assert_equal (Some "2023-06-30") (S.deadline goal);
  assert_equal [ 0.5; 0.25; 0.25 ] (S.contributions goal)


let test_load_goal_rejects_bad_rows _ =
  assert_bool "wrong number of fields should fail"
    (Result.is_error (S.load_goal "1,B,500"));
  assert_bool "invalid date should fail"
    (Result.is_error (S.load_goal "1,B,500,2025-02-29,"))

let tests =
  [
    "create_goal valid" >:: test_ok;
    "string_of_goals round trip" >:: test_string_of_goals_round_trip;
    "string_of_goals empty list" >:: test_string_of_empty_list;
    "load_goal fields" >:: test_load_goal_fields;
    "load_goal invalid rows" >:: test_load_goal_rejects_bad_rows;
  ]

let () =
  run_test_tt_main
    ("Sam: Test conversions from CSV to goals and back" >::: tests)