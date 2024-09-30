let is_pythagorian a b c = (a * a) + (b * b) = c * c

let rec find_triplet sum a b = 
  if a >= sum then
    failwith "Solution not found"
  else
    let c = sum - a - b in 
    if is_pythagorian a b c then 
      (a * b * (sum - a - b))
    else if b < sum then
      find_triplet sum a (b + 1)  
    else
      find_triplet sum (a + 1) (a + 2)

let find_triplet_rec sum = find_triplet sum 1 2
