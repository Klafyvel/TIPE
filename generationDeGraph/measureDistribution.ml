open Core.Std;;
let graph_size = 1000;;
let nb_gen = 10000;;

let save_info t =
	let file = Out_channel.create "measure.csv" in
	for i = 0 to graph_size do
		Array.iter t.(i) (fun x -> fprintf file "%d," x);
		fprintf file "\n"
	done;
	Out_channel.close file
;;

let () = 
let t = Array.create graph_size (Array.create nb_gen 0) in
for i = 1 to nb_gen do
	print_string "Graph n° ";print_int i; print_string "/"; print_int nb_gen;print_newline ();
	let g = Graph.create_random_network graph_size (graph_size/4) in
	for j = 0 to graph_size-1 do
		t.(j).(i) <- Graph.degree g.(j)
	done
done;
print_newline ();
save_info t
;;