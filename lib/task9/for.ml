let is_pythagorian a b c = (a * a) + (b * b) = c * c

let solve sum =
  let out = ref 0 in
  try
    for a = 1 to sum - 1 do
      for b = a + 1 to sum - 1 do
        let c = sum - a - b in
        if is_pythagorian a b c then (
          out := a * b * c;
          raise Exit
        )
      done
    done;
    failwith "Solution not found"
  with Exit -> !out
