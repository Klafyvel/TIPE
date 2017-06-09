open Core.Std;;
open Yojson.Basic.Util;;


type exp_stat = {
  graph_no:int;
  prop_spread:float array;
};;


let json_of_exp_stat e = 
  `Assoc [
  ("graph_no", `Int e.graph_no);
  ("prop_spread", 
    `List (List.map (Array.to_list e.prop_spread) 
      ~f:(fun x -> `Float x)))
];;

let exp_stat_of_json json =
  {
    graph_no = json |> member "graph_no" |> to_int;
    prop_spread = json |> member "prop_spread" |> to_list 
    |> List.map ~f:(fun x -> x |> to_float) |> Array.of_list
  }
;;

let escape_double_quotes s = 
  let exp = Str.regexp "\"" in
  Str.global_replace exp "\"\"" s
;;

let save_step db e exp_name= 
  json_of_exp_stat e
  |> Yojson.Basic.to_string
  |> escape_double_quotes
  |> Experiment.add_step_id db exp_name e.graph_no;
  Experiment.change_last_id db exp_name e.graph_no
;;

let process ?(max=99) db graph_size nb_gen 
k beta max_spread_step a b choose_spread name=
  let step i init= 
    let g = Graph.wattsStrogatzMatrix graph_size k beta in
    let prop_spread = Array.create max_spread_step 0.0 in
    let spread = choose_spread g init in
    let j = ref 0 in
    let p = ref (-1.0) in
    while !j <= (max_spread_step-1) && prop_spread.(!j) != !p do
      prop_spread.(!j) <- 
      (float_of_int (Spread.step_p g a b spread)) 
        /. (float_of_int graph_size);
      if !j > 0 then (p := prop_spread.(!j-1));
      incr j;
    done;
    {graph_no=i; prop_spread=prop_spread}
  in  
  let experiment init db= 
    let exp_name = Printf.sprintf 
      ("r_spreading_%s_%d_%d_%d_%d_%d_%d_%d_%d") 
      name graph_size (int_of_float (beta*.100.0)) 
      k nb_gen init max_spread_step (int_of_float a) 
        (int_of_float b) in
    print_endline ("Nom de l'expérience : "^exp_name);
    let cur_step = ref (match Experiment.get_experiment db exp_name
    with
    | None -> {graph_no= (-1);prop_spread=[||]}
    | Some(s) -> s |> Yojson.Basic.from_string |> exp_stat_of_json
    ) in
    let beg = !cur_step.graph_no + 1 in
    for i = beg to nb_gen - 1 do
      cur_step := step i ((float_of_int init)/.100.0);
      save_step db !cur_step exp_name
    done;
  in
  for i = 1 to max do
    experiment i db;
  done
;;
