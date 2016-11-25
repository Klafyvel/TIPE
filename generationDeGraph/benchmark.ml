open Core.Std
open Core_bench.Std

let graph_100 () = Graph.wattsStrogatz 100 10 0.6;;
let graph_1000 () = Graph.wattsStrogatz 1000 100 0.6;;
let graph_10000 () = Graph.wattsStrogatz 10000 100 0.6;;

let tests = [(*
  "Graphe de 100 points", graph_100;
  "Graphe de 1000 points", graph_1000;*)
  "Graphe de 10000 points", graph_10000;
]

let () = 
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run