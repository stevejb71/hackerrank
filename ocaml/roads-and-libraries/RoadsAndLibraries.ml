open Core

type city = int

type query = {
  num_cities: int;
  num_roads: int;
  library_cost: int;
  road_cost: int;
  roads: (city * city) list;
}

let parse_input (num_queries: int) (lines: string array): query list =
  let to_int_list s = String.split s ~on:' ' |> List.map ~f:Int.of_string in
  let parse1 k = function
  | num_cities :: num_roads :: library_cost :: road_cost :: [] -> {num_cities; num_roads; library_cost; road_cost; roads=k num_roads}
  | x -> failwith @@ "bad input cities " ^ (List.to_string ~f:Int.to_string x) in
  let parse_connection = function
  | c1 :: c2 :: [] -> (c1, c2)
  | x -> failwith @@ "bad input connection " ^ (List.to_string ~f:Int.to_string x) in
  let parse_query (lines: string array) (idx: int): (int * query) = 
    let header = to_int_list lines.(idx) in
    let parse_roads num_roads = 
      let rec loop acc = function
      | n when n > idx+num_roads -> acc
      | n -> loop ((parse_connection @@ to_int_list lines.(n)) :: acc) (succ n)
      in loop [] (idx+1) in
    let query = parse1 parse_roads header
    in (1+idx+query.num_roads, query) in
  let rec loop acc idx = function
  | n when n = num_queries -> acc
  | n -> let (idx, q) = parse_query lines idx in loop (q::acc) idx (n+1) 
  in loop [] 0 0 |> List.rev

let depth_first_search (start: 'a) (next: 'a -> 'a list): 'a list = 
  let rec go acc = function
  | [] -> acc
  | n::rest -> go (n::acc) (List.append (next n) rest)
  in go [] [start] |> List.rev

let dfs_step_from_roads (roads: (city * city) list) (c: city): city list = 
  List.filter_map roads ~f:(
    fun (c1, c2) -> if c1 = c 
                    then Some c2
                    else if c2 = c
                        then Some c1
                        else None)

let subtract_sorted_lists (xs: 'a list) (ys: 'a list) ~(eq: 'a -> 'a -> bool): 'a list = 
  let rec go xs ys acc = match (xs, ys) with
  | ([], _) -> List.rev acc
  | (xs, []) -> List.append (List.rev acc) xs
  | (x::xs, (y::ys as l2)) -> 
      let is_eq = eq x y in
      go xs (if is_eq then ys else l2) (if is_eq then acc else x::acc) 
  in go xs ys [] 
 
(* component, rest *)
let find_connected_component (cities: city list) (steps: city -> city list): (city list * city list) = match cities with
| [] -> ([], [])
| c::cs -> 
    let component = steps c in
    let remainder = subtract_sorted_lists cities component (=) in
    (component, remainder)

let assess_cost (q: query): int = 
  if q.library_cost <= q.road_cost
  then q.num_cities * q.library_cost
  else 0

let assess_costs_from_input (lines: string list): string =
  let num_queries = Int.of_string (List.hd_exn lines) in
  List.drop lines 1 
  |> Array.of_list 
  |> parse_input num_queries 
  |> List.map ~f:(fun q -> assess_cost q |> Int.to_string)
  |> String.concat ~sep:" "

let example_lines = ["2"; "3 3 2 1"; "1 2"; "3 1"; "2 3"; "6 6 2 5"; "1 3"; "3 4"; "2 4"; "1 2"; "2 3"; "5 6"]

let lines = example_lines |> List.tl_exn |> Array.of_list

(* let () = print_endline @@ assess_costs_from_input (In_channel.input_lines In_channel.stdin);; *)

