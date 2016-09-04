open Core.Std

let rec find_start_of_suffix (xs: 'a array) = function
  | 0 -> None
  | i when xs.(i - 1) >= xs.(i) -> find_start_of_suffix xs (i - 1)
  | i -> Some i

let rec find_swap_with_pivot (xs: 'a array) (p: int) (i: int) =
  if xs.(i) <= xs.(p)
  then find_swap_with_pivot xs p (i - 1)
  else i

let rec reverse_in_place (xs: 'a array) (i: int) (j: int) =
  if (i < j) then
    let tmp = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- tmp;
    reverse_in_place xs (i+1) (j-1)

let next_permutation (xs: 'a array) =
  let start_opt = find_start_of_suffix xs (Array.length xs - 1) in
  if Option.is_none start_opt then false
  else
    let start = Option.value_exn start_opt in
    let pivot = start - 1 in
    let swap_with_pivot = find_swap_with_pivot xs pivot (Array.length xs - 1) in
    Array.swap xs pivot swap_with_pivot;
    reverse_in_place xs start (Array.length xs - 1);
    true

let () =
  let lines = In_channel.input_lines stdin |> Array.of_list in
  let test_cases = lines.(0) |> Int.of_string in
  for i = 1 to test_cases do
    let line = String.to_array lines.(i) in
    let perm = next_permutation line in
    if perm
    then print_endline (String.of_char_list (Array.to_list line))
    else print_endline "no answer";
  done;;
