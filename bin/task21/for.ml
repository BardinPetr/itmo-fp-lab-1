let div_sum n =
  let sum = ref 0 in
  for i = 1 to n - 1 do
    if n mod i == 0 then sum := !sum + i
  done;
  !sum

let amicible_sum max =
  let sum = ref 0 in
  for i = 1 to max - 1 do
    let j = div_sum i in
    if i <> j && div_sum j = i then sum := !sum + i
  done;
  !sum

