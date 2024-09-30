(** [div_sum n] is sum of divisors of [n] *)
let div_sum n =
  List.init (n - 1) (( + ) 1)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

(** [solve n] finds sum of amicible numbers under [n] by firstly generating list
    of numbers and its sum of divisors, and then finding amicible numbers by
    searching pairs in that array*)
let solve n =
  let paired =
    List.init n (( + ) 1)
    (* claculate all sums *)
    |> List.map (fun x -> (x, div_sum x))
    (* when number equals its sum of divs, it is can't have a pair *)
    |> List.filter (fun (a, b) -> a <> b)
  in
  paired
  |> List.filter_map (fun (a, b) ->
         (* find such pair that is reverse of our, what means that other number has sum equals to our number*)
         paired |> List.find_opt (( = ) (b, a)) |> Option.map (fun (a, _) -> a))
  |> List.fold_left ( + ) 0
