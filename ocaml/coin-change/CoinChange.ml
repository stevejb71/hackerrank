open Core

let make_change ~(target: int) ~(coins: int array): int64 = 
  let mk_row = function 
    | 0 -> Array.create ~len:(1+target) 0L
    | _ -> Array.init (1+target) ~f:(function | 0 -> 1L | _ -> 0L) in
  (* A table with 
     columns - the target amounts, from 0 up to the final target
     rows -    the coins
     so table.(c_i).(target) = #ways of making target using only the first c_i coins.
  *)
  let table = Array.init (1+Array.length coins) ~f:mk_row in
  let fill_row c_i coin =
    let c_i = c_i + 1 in
    for t = 1 to target do
      let other_coins_count = table.(c_i - 1).(t) in
      let other_target_count = if t >= coin then table.(c_i).(t-coin) else 0L in
      table.(c_i).(t) <- Int64.(other_coins_count + other_target_count);
    done; in
  Array.iteri coins ~f:fill_row;
  table.(Array.length coins).(target)

let () =
  let input = In_channel.input_lines In_channel.stdin |> Array.of_list in
  let target = String.split ~on:' ' input.(0) |> List.hd_exn |> Int.of_string in
  let coins = Array.map ~f:Int.of_string (String.split ~on:' ' input.(1) |> Array.of_list) in
  make_change ~target ~coins |> Int64.to_string |> print_endline