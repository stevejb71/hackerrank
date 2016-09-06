open Core.Std

let int_bit_to_char = function
  | 0 -> '0'
  | 1 -> '1'
  | _ -> failwith "bad bit"
;;

let char_bit_to_int = function
  | '0' -> 0
  | '1' -> 1
  | _ -> failwith "bad bit"
;;

let decode (k: int) (s: string): string =
  let bits = String.to_array s |> Array.map ~f:char_bit_to_int in
  let out = Array.create ~len:(String.length s + 1 - k) 0 in
  let curr = ref 0 in
  out.(0) <- bits.(0);
  for pos = 0 to (String.length s - k - 1) do
    let out_bit = if pos < k - 1 then 0 else out.(pos-(k-1)) in
    curr := !curr lxor out_bit lxor out.(pos);
    out.(pos+1) <- !curr lxor bits.(pos+1);
  done;
  Array.map ~f:int_bit_to_char out |> Array.to_list |> String.of_char_list
;;

let () =
  let lines = In_channel.input_lines stdin |> Array.of_list in
  let k = lines.(0)
            |> String.split ~on:' '
            |> List.filter ~f:(Fn.non String.is_empty)
            |> List.tl_exn
            |> List.hd_exn
            |> Int.of_string in
  let s = lines.(1) in
  decode k s |> print_endline
;;
