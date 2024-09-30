let div_sum n =
  List.init (n - 1) (( + ) 1)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

let solve n =
  let paired =
    List.init n (( + ) 1)
    |> List.map (fun x -> (x, div_sum x))
    |> List.filter (fun (a, b) -> a <> b)
  in
  paired
  |> List.filter_map (fun (a, b) ->
         paired |> List.find_opt (( = ) (b, a)) |> Option.map (fun (a, _) -> a))
  |> List.fold_left ( + ) 0
