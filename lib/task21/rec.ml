(** [div_sum n] is sum of divisors of [n] calculated by recursively iterating
    numbers under [n] *)
let div_sum n =
  let rec div_sum_rec n cur =
    if cur >= n then
      0
    else
      (* if this is divisor add it to sum of divisors greater *)
      (if n mod cur = 0 then
         cur
       else
         0)
      + div_sum_rec n (cur + 1)
  in
  div_sum_rec n 1

(** [solve max] finds sum of amicible numbers under [max] by checking all
    numbers to have pair with [div_sum]*)
let solve max =
  let rec amicible_sum_aux cur =
    if cur >= max then
      0
    else
      let cur_sum = div_sum cur in
      (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
      (if cur <> cur_sum && div_sum cur_sum = cur then
         cur
       else
         0)
      + amicible_sum_aux (cur + 1)
  in
  amicible_sum_aux 1
