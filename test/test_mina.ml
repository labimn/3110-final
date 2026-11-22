open OUnit2
module T = Spendwise.Transaction
module S = Spendwise.Savings

let ok = function Ok value -> value | Error message -> assert_failure message
let error result = assert_bool "expected an error" (Result.is_error result)

let money expected actual =
  assert_equal
    ~cmp:(fun a b -> abs_float (a -. b) < 0.000001)
    ~printer:string_of_float expected actual

let goal ?(id = 1) ?(name = "Vacation") ?(target = 1000.) ?(deadline = None) ()
    =
  ok (S.create_goal ~id ~name ~target ~deadline)

let find id goals =
  match S.get_goal id goals with
  | Some value -> value
  | None -> assert_failure "missing goal"

let case name f = name >:: fun _ -> f ()

let parser_tests =
  List.map
    (fun input ->
      case ("invalid amount: " ^ input) (fun () -> error (T.parse_amount input)))
    [ ""; " "; "0"; "-0"; "-5"; "abc"; "nan"; "infinity"; "-infinity"; "1e999" ]
  @ List.map
      (fun (input, expected) ->
        case ("valid amount: " ^ input) (fun () ->
            money expected (ok (T.parse_amount input))))
      [ (" 25.50 ", 25.5); ("0.01", 0.01); ("1000", 1000.) ]
  @ List.map
      (fun input ->
        case ("invalid date: " ^ input) (fun () -> error (T.parse_date input)))
      [
        "";
        "2026-1-01";
        "2026-01-1";
        "0000-01-01";
        "2026-00-01";
        "2026-13-01";
        "2026-01-00";
        "2026-04-31";
        "1900-02-29";
        "2025-02-29";
        "2026-01-32";
        "2026/01/01";
        "2026-01-01-extra";
        "+026-01-01";
        "2026-+1-01";
        "2026-01-+1";
        "2026- 1-01";
      ]
  @ List.map
      (fun input ->
        case ("valid date: " ^ input) (fun () ->
            assert_equal input (ok (T.parse_date input))))
      [ "2000-02-29"; "2024-02-29"; "2026-02-28"; "2026-12-31"; "0001-01-01" ]
  @ [
      case "trim date" (fun () ->
          assert_equal "2026-11-20" (ok (T.parse_date " 2026-11-20 ")));
      case "text whitespace rejected" (fun () ->
          error (T.parse_text "Description" " \t\n"));
      case "text trimmed" (fun () ->
          assert_equal "Lunch" (ok (T.parse_text "Description" " Lunch ")));
      case "type trimmed and case insensitive" (fun () ->
          assert_equal T.Income (ok (T.parse_transaction_type " INCOME "));
          assert_equal T.Expense (ok (T.parse_transaction_type " Expense ")));
      case "empty type rejected" (fun () ->
          error (T.parse_transaction_type " "));
    ]

let savings_tests =
  [
    case "new goal starts empty" (fun () ->
        let g = goal ~name:" Vacation " () in
        assert_equal 1 (S.id g);
        assert_equal "Vacation" (S.name g);
        assert_equal None (S.deadline g);
        assert_equal [] (S.contributions g);
        let p = S.progress g in
        money 0. p.saved;
        money 1000. p.remaining;
        money 0. p.percent;
        assert_bool "incomplete" (not p.complete));
    case "deadline validated and trimmed" (fun () ->
        assert_equal (Some "2024-02-29")
          (S.deadline (goal ~deadline:(Some " 2024-02-29 ") ()));
        List.iter
          (fun date ->
            error
              (S.create_goal ~id:1 ~name:"Trip" ~target:10.
                 ~deadline:(Some date)))
          [ ""; "2025-02-29"; "2026-13-01"; "+026-01-01" ]);
    case "invalid goal fields" (fun () ->
        List.iter
          (fun target ->
            error (S.create_goal ~id:1 ~name:"Trip" ~target ~deadline:None))
          [ 0.; -1.; nan; infinity; neg_infinity ];
        List.iter
          (fun id ->
            error (S.create_goal ~id ~name:"Trip" ~target:10. ~deadline:None))
          [ 0; -1 ];
        error (S.create_goal ~id:1 ~name:" \t " ~target:10. ~deadline:None));
    case "multiple independent goals" (fun () ->
        let a = goal () and b = goal ~id:2 ~name:"Laptop" ~target:2000. () in
        let goals = ok (S.add_goal b (ok (S.add_goal a []))) in
        assert_equal [ 1; 2 ] (List.map S.id goals);
        error (S.add_goal a goals);
        assert_equal None (S.get_goal 99 goals);
        assert_equal 3 (ok (S.next_goal_id goals));
        assert_equal 1 (ok (S.next_goal_id []));
        let updated = ok (S.contribute 1 ~amount:250. goals) in
        let p = S.progress (find 1 updated) in
        money 250. p.saved;
        money 750. p.remaining;
        money 25. p.percent;
        money 0. (S.progress (find 2 updated)).saved;
        money 0. (S.progress a).saved);
    case "contributions accumulate and complete" (fun () ->
        let goals = ok (S.contribute 1 ~amount:250. [ goal () ]) in
        let goals = ok (S.contribute 1 ~amount:750. goals) in
        let g = find 1 goals in
        assert_equal [ 250.; 750. ] (S.contributions g);
        let p = S.progress g in
        money 1000. p.saved;
        money 0. p.remaining;
        money 100. p.percent;
        assert_bool "complete" p.complete);
    case "overfunding clamps display" (fun () ->
        let goals = ok (S.contribute 1 ~amount:1250. [ goal () ]) in
        let p = S.progress (find 1 goals) in
        money 1250. p.saved;
        money 0. p.remaining;
        money 100. p.percent;
        assert_bool "complete" p.complete);
    case "invalid contributions leave original intact" (fun () ->
        let original = [ goal () ] in
        List.iter
          (fun amount -> error (S.contribute 1 ~amount original))
          [ 0.; -1.; nan; infinity; neg_infinity ];
        error (S.contribute 99 ~amount:10. original);
        money 0. (S.progress (find 1 original)).saved);
    case "overflow rejected" (fun () ->
        let goals =
          ok (S.contribute 1 ~amount:90_000_000_000_000. [ goal () ])
        in
        error (S.contribute 1 ~amount:0.01 goals);
        money 100. (S.progress (find 1 goals)).percent);
    case "edit preserves history and can clear deadline" (fun () ->
        let g = goal ~deadline:(Some "2027-01-01") () in
        let goals = ok (S.contribute 1 ~amount:250. [ g; goal ~id:2 () ]) in
        let edited =
          ok
            (S.edit_goal 1 ~name:" New goal " ~target:200. ~deadline:None goals)
        in
        let g = find 1 edited in
        assert_equal "New goal" (S.name g);
        assert_equal 1 (S.id g);
        assert_equal None (S.deadline g);
        assert_equal [ 250. ] (S.contributions g);
        money 0. (S.progress g).remaining;
        money 100. (S.progress g).percent;
        let raised =
          ok
            (S.edit_goal 1 ~name:"New goal" ~target:500.
               ~deadline:(Some "2027-02-28") edited)
        in
        money 50. (S.progress (find 1 raised)).percent;
        money 0. (S.progress (find 2 raised)).saved);
    case "invalid edit and missing goals" (fun () ->
        let goals = [ goal () ] in
        error (S.edit_goal 1 ~name:"" ~target:100. ~deadline:None goals);
        error (S.edit_goal 1 ~name:"Trip" ~target:0. ~deadline:None goals);
        error (S.edit_goal 99 ~name:"Trip" ~target:100. ~deadline:None goals);
        error (S.delete_goal 99 goals));
    case "delete preserves other goals" (fun () ->
        let goals = ok (S.delete_goal 1 [ goal (); goal ~id:2 () ]) in
        assert_equal [ 2 ] (List.map S.id goals);
        assert_equal [] (ok (S.delete_goal 2 goals)));
    case "ID exhaustion" (fun () ->
        error (S.next_goal_id [ goal ~id:max_int () ]));
    case "cent arithmetic completes exactly" (fun () ->
        let goals = ok (S.contribute 1 ~amount:0.10 [ goal ~target:0.80 () ]) in
        let goals = ok (S.contribute 1 ~amount:0.70 goals) in
        let p = S.progress (find 1 goals) in
        assert_bool "10 + 70 cents reaches 80 cents" p.complete;
        money 0.80 p.saved;
        money 0. p.remaining;
        money 100. p.percent);
    case "fractional cents rejected" (fun () ->
        error (S.create_goal ~id:1 ~name:"Trip" ~target:0.001 ~deadline:None);
        error (S.contribute 1 ~amount:1.001 [ goal () ]));
    case "fractional contributions" (fun () ->
        let goals = ok (S.contribute 1 ~amount:0.25 [ goal ~target:1. () ]) in
        let goals = ok (S.contribute 1 ~amount:0.5 goals) in
        let p = S.progress (find 1 goals) in
        money 0.75 p.saved;
        money 0.25 p.remaining;
        money 75. p.percent);
  ]

let integration_tests =
  [
    case "summary follows transaction edits and deletion" (fun () ->
        let module B = Spendwise.Budget in
        let budget =
          ok (B.create_budget ~month:"2026-11" ~category:"Food" ~limit:100.)
        in
        let expense =
          T.create_transaction 1 25. "2026-11-20" "Lunch" "Food" T.Expense
        in
        let income =
          T.create_transaction 2 500. "2026-11-20" "Salary" "Work" T.Income
        in
        let transactions = [ expense; income ] in
        money 25. (B.summarize budget transactions).spent;
        let replacement = { expense with amount = 80. } in
        let edited = ok (T.edit_transaction 1 replacement transactions) in
        let summary = B.summarize budget edited in
        money 80. summary.spent;
        money 20. summary.remaining;
        assert_equal B.Approaching_limit summary.warning;
        let deleted = ok (T.delete_transaction 1 edited) in
        money 0. (B.summarize budget deleted).spent;
        money 100. (B.summarize budget deleted).remaining);
    case "savings allocation does not alter expense summaries" (fun () ->
        let module B = Spendwise.Budget in
        let budget =
          ok (B.create_budget ~month:"2026-11" ~category:"Food" ~limit:100.)
        in
        let transactions =
          [ T.create_transaction 1 25. "2026-11-20" "Lunch" "Food" T.Expense ]
        in
        let goals = ok (S.contribute 1 ~amount:250. [ goal () ]) in
        money 250. (S.progress (find 1 goals)).saved;
        money 25. (B.summarize budget transactions).spent;
        money 75. (B.summarize budget transactions).remaining);
  ]

let () =
  run_test_tt_main
    ("Mina: parser and savings"
    >::: parser_tests @ savings_tests @ integration_tests)
