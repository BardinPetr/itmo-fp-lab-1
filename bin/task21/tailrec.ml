let rec div_sum n =
  let rec div_sum_aux cur acc = 
    if cur >= n then acc
    else div_sum_aux (cur + 1) (acc + if n mod cur = 0 then cur else 0)
  in div_sum_aux 1 0

let rec amicible_sum max = 
  let rec amicible_sum_aux cur acc = 
    if cur >= max then acc
    else     
      let pair = div_sum cur in
      amicible_sum_aux (cur + 1) (
        if cur <> pair && (div_sum pair = cur) then acc + cur else acc
      ) 
  in amicible_sum_aux 1 0
