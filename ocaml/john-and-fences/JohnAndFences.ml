open Core.Std

let max_carving_area_at (hs: int array) (i: int): int =
  let h = hs.(i) in
  let rec find_level_index (start: int) (delta: int) (end_point: int): int =
    let next = start + delta in
    if next = end_point
    then start
    else if hs.(next) >= h then find_level_index next delta end_point else start in
  let r = find_level_index i 1 (Array.length hs) in
  let l = find_level_index i (-1) (-1) in
  h * (r - l + 1)
;;
  
let max_carving_area (hs: int array): int =
  Sequence.range 0 (Array.length hs) 
  |> Sequence.map ~f:(max_carving_area_at hs)
  |> Sequence.max_elt ~cmp:Int.compare
  |> Option.value_exn 
;;

let list_of_ints (s: string): int list =
  String.split ~on:' ' s |> List.map ~f:Int.of_string
;;
  
let () =
  In_channel.input_lines stdin
  |> List.tl_exn
  |> List.hd_exn
  |> list_of_ints
  |> Array.of_list
  |> max_carving_area
  |> print_int
;;
