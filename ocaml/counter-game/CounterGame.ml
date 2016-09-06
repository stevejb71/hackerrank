open Core.Std

let other_player = function
  | p when p = "Richard" -> "Louise"
  | _ -> "Richard"

let two = Big_int.big_int_of_int 2

let power_of_2_lower_than (n: Big_int.big_int): Big_int.big_int =
  let rec go (pow: int) =
    if Big_int.gt_big_int (Big_int.power_int_positive_int 2 pow) n
    then Big_int.power_int_positive_int 2 (pow - 1)
    else go (pow + 1) in
  go 0

let rec find_winner (player: string) (n: Big_int.big_int): string =
  if Big_int.eq_big_int n Big_int.unit_big_int then
    other_player player
  else
    (* let nearest_power_of_2 = Big_int.power_big_int_positive_int two (Big_int.num_bits_big_int n |> pred) in *)
    let nearest_power_of_2 = power_of_2_lower_than n in
    let next_n = if Big_int.eq_big_int nearest_power_of_2 n
                    then Big_int.shift_right_big_int n 1
                    else Big_int.sub_big_int n nearest_power_of_2 in
    find_winner (other_player player) next_n

let () =
  let lines = In_channel.input_lines stdin |> Array.of_list in
  let test_cases = lines.(0) |> Int.of_string in
  for i = 1 to test_cases do
    lines.(i) |> Big_int.big_int_of_string |> find_winner "Louise" |> print_endline;
  done;;
