open Core.Std

let find_missing_numbers (a: int list) (b: int list): int list =
  let counts = Array.create ~len:(10101 * 2) 0 in
  let xmin = ref 10102 in
  List.iter b ~f:(fun i ->
              counts.(i + 10101) <- counts.(i + 10101) + 1;
              xmin := min !xmin i
            ); 
  List.iter a ~f:(fun i ->
              counts.(i + 10101) <- counts.(i + 10101) - 1;             
            );
  let missing_counts = Array.slice counts (10101 + !xmin) (10101 + !xmin + 101) in
  let missing_lists = Array.foldi missing_counts ~init:[] ~f:(fun i acc n ->
                                   if n > 0 then [!xmin + i] :: acc else acc) in
  List.concat missing_lists |> List.sort ~cmp:Int.compare
                
let list_of_ints (s: string): int list =
  String.split ~on:' ' s |> List.map ~f:Int.of_string

let print_list xs = List.map xs ~f:Int.to_string |> String.concat ~sep:" " |> print_string;;
                                     
let () =
  let lines = In_channel.input_lines stdin |> Array.of_list in
  let a = list_of_ints lines.(1) in
  let b = list_of_ints lines.(3) in
  let missing = find_missing_numbers a b in
  print_list missing
;;
