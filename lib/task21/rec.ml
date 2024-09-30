let rec div_sum_rec n cur =
  if cur >= n then
    0
  else
    (if n mod cur = 0 then
       cur
     else
       0)
    + div_sum_rec n (cur + 1)

let div_sum n = div_sum_rec n 1

let solve max =
  let rec amicible_sum_aux cur =
    if cur >= max then
      0
    else
      let cur_sum = div_sum cur in
      (if cur <> cur_sum && div_sum cur_sum = cur then
         cur
       else
         0)
      + amicible_sum_aux (cur + 1)
  in
  amicible_sum_aux 1
