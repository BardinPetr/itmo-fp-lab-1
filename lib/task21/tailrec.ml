(** [div_sum n] is sum of divisors of [n] implemented with tail recursion by
    accumulating sum*)
let div_sum n =
  let rec div_sum_aux cur acc =
    if cur >= n then
      acc
    else
      div_sum_aux (cur + 1)
        (acc
        +
        if n mod cur = 0 then
          cur
        else
          0)
  in
  div_sum_aux 1 0

(** [solve max] finds sum of amicible numbers under [max] implemented with tail
    recursion by accumulating sum of numbers under [max] that are amicible bt
    matching with [div_sum]*)
let solve max =
  let rec amicible_sum_aux cur acc =
    if cur >= max then
      acc
    else
      (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
      let pair = div_sum cur in
      amicible_sum_aux (cur + 1)
        (* for amicible pair add number to sum of amicible pairs greater that that *)
        (if cur <> pair && div_sum pair = cur then
           acc + cur
         else
           acc)
  in
  amicible_sum_aux 1 0
