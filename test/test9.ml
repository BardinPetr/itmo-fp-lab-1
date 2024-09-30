let task9_implementations =
  [
    ("Tail-recursive algorithm", Task9.Tailrec.solve);
    ("Module implementation", Task9.Module.solve);
    ("Map based implementation", Task9.Map.solve);
    ("For loop based implementation", Task9.For.solve);
    ("Infinite sequences based algorithm", Task9.Inf.solve);
  ]

let task9_checker solve_fun () =
  Alcotest.(check int) "Valid output" 31875000 (solve_fun 1000)
