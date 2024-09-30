(** [triplet_list sum] is a list of triplets ([a],[b],[c]) where [a] < [b] < [c]
    and ([a] + [b] + [c] = [sum]) *)
let gen_triplet_list sum =
  let rec triplet_list_aux sum a b acc =
    let c = sum - a - b in
    if a >= sum / 3 then
      (* limit with sum/3+(sum/3+1)+(sum/3+2) = sum+3 *)
      acc
    else if b >= c then
      (* go to next a, and b after a *)
      triplet_list_aux sum (a + 1) (a + 2) ((a, b, c) :: acc)
    else
      triplet_list_aux sum a (b + 1) ((a, b, c) :: acc)
  in
  triplet_list_aux sum 1 2 []

(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian (a, b, c) = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by firstly
    generating list of triplets matching restrictions over a,b,c and then
    filtering with [is_pythagorian]*)
let solve sum =
  sum
  |> gen_triplet_list
  |> List.filter is_pythagorian
  |> List.hd
  |> fun (x, y, z) -> x * y * z
