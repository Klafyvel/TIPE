
let graph_size = 500;;
let nb_gen = 50;;
let max_spread_step = 500;;
let b = 0.5;;

let () = 
  print_endline "Initialisation de Random.";
  Random.self_init ();
  print_endline "Ouverture de la base de données.";
  let db = Experiment.load_db () in
  ExperimentSpreading.process ~max:50 db graph_size nb_gen 50 
    b max_spread_step 2.0 1.0 ExperimentSpreadingRandom.choose_spread "random";
  ExperimentSpreading.process ~max:50 db graph_size nb_gen 50 
    b max_spread_step 2.0 1.0 ExperimentSpreadingDegree.choose_spread "degree";
  ExperimentSpreading.process ~max:50 db graph_size nb_gen 50 
    b max_spread_step 2.0 1.0 ExperimentSpreadingBetween.choose_spread "between";
  print_endline "Fermeture de la base de données.";
  if (Experiment.close_db db) then
    print_endline "Fermeture réussie."
  else
    print_endline "Echec de la fermeture."
