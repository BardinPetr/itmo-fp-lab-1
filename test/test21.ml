let task21_implementations =
  [
    ("Recursive algorithm", Task21.Rec.solve);
    ("Tail-recursive algorithm", Task21.Tailrec.solve);
    ("Module implementation", Task21.Module.solve);
    ("Map based implementation", Task21.Map.solve);
    ("For loop based implementation", Task21.For.solve);
  ]

let task21_checker solve_fun () =
  Alcotest.(check int) "Valid output" 31626 (solve_fun 10000)
