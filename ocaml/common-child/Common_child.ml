open Core.Std

let nested_string_iteri (s1: string) (s2: string) (f: int -> char -> int -> char -> unit): unit =
  for i1 = 0 to String.length s1 - 1 do
    for i2 = 0 to String.length s2 - 1 do
      f i1 s1.[i1] i2 s2.[i2]
    done
  done;;
        
let common_child (s1: string) (s2: string): int =
  let m = Array.make_matrix ~dimx:(1 + String.length s1) ~dimy:(1 + String.length s2) 0 in
  nested_string_iteri s1 s2 (fun i1 c1 i2 c2 ->
                        m.(i1+1).(i2+1) <- if c1 = c2 then m.(i1).(i2) + 1 else max m.(i1+1).(i2) m.(i1).(i2+1));
  m.(String.length s1).(String.length s2);;
                    
let () =
  let strs = Array.of_list (In_channel.input_lines stdin) in
  let s1 = strs.(0) in
  let s2 = strs.(1) in
  print_endline (common_child s1 s2 |> Int.to_string)
