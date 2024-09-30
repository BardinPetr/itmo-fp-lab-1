let div_sum n =
  List.init (n - 1) (( + ) 1)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

let is_amicible n =
  if n < 2 then
    false
  else
    let pair = div_sum n in
    n <> pair && n = div_sum pair

let solve n =
  List.init n (( + ) 1)
  |> List.filter (fun x -> is_amicible x)
  |> List.fold_left ( + ) 0
