open Core.Std;;

let graph_size = 500;;
let nb_gen = 100;;
let max_spread_step = 500;;

let choose_spread g init = 
    let s = Array.create graph_size false in
    let n = int_of_float ((float_of_int graph_size) *. init) in
    let rec loop l = match l with
    | [] -> ()
    | hd::tl -> s.(hd) <- true; loop tl
    in
    loop (Graph.maxBetweenness g n); s
;;

let () = 
  print_endline "Initialisation de Random.";
  Random.self_init ();
  print_endline "Ouverture de la base de données.";
  let db = Experiment.load_db () in
  for b = 0 to 4 do
    ExperimentSpreading.process db graph_size nb_gen 50 
      ((float_of_int b) *. 0.25) max_spread_step 1.0 1.0 choose_spread "between";
    ExperimentSpreading.process db graph_size nb_gen 50 
      ((float_of_int b) *. 0.25) max_spread_step 3.0 1.0 choose_spread "between";
    ExperimentSpreading.process db graph_size nb_gen 50 
      ((float_of_int b) *. 0.25) max_spread_step 1.0 3.0 choose_spread "between";
  done;
  print_endline "Fermeture de la base de données.";
  if (Experiment.close_db db) then
    print_endline "Fermeture réussie."
  else
    print_endline "Echec de la fermeture."
;;
