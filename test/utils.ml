let run_with_save_report package_name test_name report_file tests =
  let output, _ =
    Junit_alcotest.run_and_report ~package:package_name test_name tests
  in
  match report_file with
  | Some path -> ignore (([ output ] |> Junit.make |> Junit.to_file) path)
  | None -> ()
