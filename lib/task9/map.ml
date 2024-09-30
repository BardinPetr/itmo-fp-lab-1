(** [solve sum] searches for pythagorian triplet with items sum equals [sum] *)
let solve sum =
  List.init (sum - 2) (( + ) 1)
  (* get all pairs where [a] < [b] < [sum] *)
  |> List.map (fun a -> List.init (sum - a - 1) (fun b -> (a, a + b + 1)))
  |> List.flatten
  (* calculate [c] from [sum] *)
  |> List.map (fun (a, b) -> (a, b, sum - a - b))
  (* filter by [a]<[b]<[c] *)
  |> List.filter (fun (_, b, c) -> b < c)
  (* calculate product for triples that matches pythongorial equation *)
  |> List.filter_map (fun (a, b, c) ->
         if (a * a) + (b * b) == c * c then
           Some (a * b * c)
         else
           None)
  |> List.hd
