open Core.Std;;
open Yojson.Basic.Util;;

let graph_size = 500;;
let nb_gen = 100;;
let k = 50;;
let beta = 0.6;;

type exp_stat = {
  graph_no:int;
  btc:float array
};;

let exp_name = 
  "betweenness_wattsstrogatz_"^(string_of_int graph_size)^"_"^(string_of_int k)^"_"^(string_of_int nb_gen);;

let exp_info =
"Watts-Strogatz,"^(string_of_int graph_size)^"noeuds, "^(string_of_int nb_gen)^"generations, k="^
(string_of_int k)^", beta="^(string_of_float beta)^"."
;;

let json_of_exp_stat e =
  `Assoc [
    ("graph_no", `Int e.graph_no);
    ("btc", `List (List.map (Array.to_list e.btc) ~f:(fun x -> `Float x)))
  ]
;;

let exp_stat_of_json json = 
  {
    graph_no=json |> member "graph_no" |> to_int;
    btc = json |> member "btc" |> to_list |> List.map ~f:(fun x -> x |> to_float) |> Array.of_list
  }
;;

let escape_double_quotes s = 
  let exp = Str.regexp "\"" in
  Str.global_replace exp "\"\"" s
;;

let save_step db e = 
  json_of_exp_stat e
  |> Yojson.Basic.to_string
  |> escape_double_quotes
  |> Experiment.add_step_id db exp_name (string_of_int e.graph_no);
  Experiment.change_last_id db exp_name e.graph_no
;;


let print_matrix m = let n = Array.length m in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if m.(i).(j) then print_string "1 " else print_string "0 "
    done;
    print_newline ();
  done;
  print_newline();
;;


let step i = 
  let g = Graph.wattsStrogatzMatrix graph_size k beta in
  (*print_matrix g;*)
  let btc = Graph.betweenness g in
  {
    graph_no=i;
    btc=btc
  }

;;


let () = 
  print_endline "Ouverture de la base de données.";
  print_endline ("Nom de l'expérience : "^exp_name);
  let db = Experiment.load_db () in
  let cur_step = ref (match Experiment.get_experiment db exp_name
  with
  | None -> {graph_no= (-1);btc=[||]}
  | Some(s) -> s |> Yojson.Basic.from_string |> exp_stat_of_json
  ) in
  let beg = !cur_step.graph_no + 1 in
  for i = beg to nb_gen - 1 do
    print_endline ("Progression :"^(string_of_int i)^"/"^(string_of_int nb_gen));
    cur_step := step i;
    save_step db !cur_step
  done;
  if (Experiment.close_db db) then
    print_endline "Fermeture réussie."
  else
    print_endline "Echec de la fermeture."
;;


