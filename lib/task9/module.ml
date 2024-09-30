(** [triplet_list sum] is a list of triplets ([a],[b],[c]) where [a] < [b] < [c]
    and ([a] + [b] + [c] = [sum]) *)
let gen_triplet_list sum =
  let rec triplet_list_aux sum a b acc =
    let c = sum - a - b in
    if a >= sum / 3 then
      (* sum/3+(sum/3+1)+(sum/3+2) = sum+3 *)
      acc
    else if b >= c then
      triplet_list_aux sum (a + 1) (a + 2) ((a, b, c) :: acc)
    else
      triplet_list_aux sum a (b + 1) ((a, b, c) :: acc)
  in
  triplet_list_aux sum 1 2 []

let is_pythagorian (a, b, c) = (a * a) + (b * b) = c * c

let solve sum =
  sum |> gen_triplet_list |> List.filter is_pythagorian |> List.hd
  |> fun (x, y, z) -> x * y * z
