let is_pythagorian (a, b, c) = (a * a) + (b * b) = c * c

(** [generate_inf_pairs] is infinite sequence of infinite sequences of integer
    pairs ([a], [b]) such that [a] < [b] *)
let generate_inf_pairs =
  Seq.ints 1
  |> Seq.map (fun a -> a + 1 |> Seq.ints |> Seq.map (fun b -> (a, b)))

(** [to_triple_with_sum sum (a, b)] is triple sum of which is [sum]*)
let to_triple_with_sum sum (a, b) = (a, b, sum - a - b)

(** [generate_triples_up_to_sum sum] if finite sequence of integer triples ([a],
    [b], [c]) such that [a] < [b] < [c] limited by ([a]+[b]+[c]) = [sum] *)
let generate_triples_up_to_sum sum =
  generate_inf_pairs
  (* limit a *)
  |> Seq.take sum
  (* limit b and c by cheching a < b < c *)
  |> Seq.flat_map (fun i ->
         i
         |> Seq.map (to_triple_with_sum sum)
         |> Seq.take_while (fun (_, b, c) -> b < c))

let solve sum =
  generate_triples_up_to_sum sum
  |> Seq.find is_pythagorian
  |> Option.map (fun (x, y, z) -> x * y * z)
  |> Option.get
