open Core.Std;;
let graph_size = 500;;
let nb_gen = 100;;

let save_info t =
  let file = Out_channel.create "testCentralite.csv" in
  Array.iter t (fun x -> fprintf file "%f,\n" x); 
  Out_channel.close file
;;

let () = 
let t = Array.create graph_size 0.0 in
for i = 1 to nb_gen do
  print_string "Graph n° ";print_int i; print_string "/"; print_int nb_gen;print_newline ();
  let g = Graph.wattsStrogatzMatrix graph_size 10 0.6 in
  let btc = Graph.betweenness g in
  for j = 0 to graph_size-1 do
    t.(j) <- t.(j) +. btc.(j) /. (float_of_int nb_gen)
  done
done;
print_newline ();
save_info t
;;
