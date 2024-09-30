(** [div_sum n] is sum of divisors of [n] *)
let div_sum n =
  List.init (n - 1) (( + ) 1)
  (* iterate over list of numbers under [n] and sum all that divides *)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

(** [is_amicible n] checks if [n] is amicibly by calling [div_sum]*)
let is_amicible n =
  if n < 2 then
    false
  else
    (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
    let pair = div_sum n in
    n <> pair && n = div_sum pair

(** [solve max] finds sum of amicible numbers under [max]*)
let solve max =
  List.init max (( + ) 1)
  |> List.filter (fun x -> is_amicible x)
  |> List.fold_left ( + ) 0
