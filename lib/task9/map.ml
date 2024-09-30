(** [solve sum] searches for pythagorian triplet with items sum equals [sum] by
    algorithm: 1. get all pairs where [a] < [b] < [sum] 2. calculate [c] from
    [sum] 3. filter by [a]<[b]<[c] 4. calculate product for triples that matches
    pythongorial equation *)
let solve sum =
  List.init (sum - 2) (( + ) 1)
  |> List.map (fun a -> List.init (sum - a - 1) (fun b -> (a, a + b + 1)))
  |> List.flatten
  |> List.map (fun (a, b) -> (a, b, sum - a - b))
  |> List.filter (fun (_, b, c) -> b < c)
  |> List.filter_map (fun (a, b, c) ->
         if (a * a) + (b * b) == c * c then
           Some (a * b * c)
         else
           None)
  |> List.hd
