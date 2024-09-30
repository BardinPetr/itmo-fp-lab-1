(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian a b c = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by iterating
    [a] and [b] in bounds of [sum] and checking [is_pythagorian]
    @return product of triplet components*)
let solve sum =
  (* as OCaml do not have loop exit command,
      we need to store result in variable by reference*)
  let out = ref 0 in
  try
    for a = 1 to sum - 1 do
      for b = a + 1 to sum - 1 do
        let c = sum - a - b in
        if is_pythagorian a b c then (
          out := a * b * c;
          (* raise error to stop iteration, as by task there is only one such triplet*)
          raise Exit
        )
      done
    done;
    failwith "Solution not found"
  with Exit -> !out
