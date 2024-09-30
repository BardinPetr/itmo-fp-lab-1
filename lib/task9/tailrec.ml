(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian a b c = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by
    recursively iterating upwards over [b] if possible, then over [a] and so on,
    then checking [is_pythagorian] for [c] calculated from sum*)
let solve sum =
  let rec find_triplet sum a b =
    if a >= sum then
      failwith "Solution not found"
    else
      let c = sum - a - b in
      if is_pythagorian a b c then
        a * b * (sum - a - b)
      else if b < sum then
        find_triplet sum a (b + 1)
      else
        find_triplet sum (a + 1) (a + 2)
  in
  find_triplet sum 1 2
