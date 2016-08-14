open Core.Std

let common_child (s1: string) (s2: string): int =
  let rec go (s1: string) (s2: string) (result: int): int =
    if String.is_empty s1 then result
    else match String.index s2 s1.[0] with
      | None -> go (String.drop_prefix s1 1) s2 result
      | Some(i2) ->
         let m1 = go (String.drop_prefix s1 1) (String.drop_prefix s2 (i2+1)) (1 + result) in
         let m2 = go (String.drop_prefix s1 1) s2 result in
         max m1 m2
  in go s1 s2 0

let () =
  let strs = Array.of_list (In_channel.input_lines stdin) in
  let s1 = strs.(0) in
  let s2 = strs.(1) in
  print_endline (common_child s1 s2 |> Int.to_string)
