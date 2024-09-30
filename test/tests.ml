open Test9
open Test21

let testset_from_impls checker =
  List.map (fun (name, solve) -> Alcotest.test_case name `Quick (checker solve))

let report_path = Sys.getenv_opt "REPORT_PATH"

let () =
  Utils.run_with_save_report "euler" "Euler problems solutions test" report_path
    [
      ("Task #9", testset_from_impls task9_checker task9_implementations);
      ("Task #21", testset_from_impls task21_checker task21_implementations);
    ]
