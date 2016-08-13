open Core.Std

let (<<) f g = Fn.compose f g

let add_to_counting_map =
  Map.change ~f:(function None -> Some 1 | Some x -> Some (x + 1))

let read_arguments args =
  let n = Int.of_string args.(0) in
  let strings = Array.slice args 1 (n + 1) in
  let counted_strings = Array.fold strings ~init:String.Map.empty ~f:add_to_counting_map in
  let q = Int.of_string args.(n + 1) in
  let queries = Array.slice args (n + 2) (n + 2 + q) in
  (counted_strings, queries)

let solve (args: string array) =
  let (counted_strings, queries) = read_arguments args in
  let counts = Array.map queries ~f:(Option.value ~default:0 << Map.find counted_strings) in
  Array.iter counts ~f:(print_endline << Int.to_string)

let () =
  In_channel.input_lines stdin |> Array.of_list |> solve;;
