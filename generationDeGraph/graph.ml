type graph_element = {id:int;mutable link:int list;degree: int ref};;

let make_empty_node ()= {id=0;link=[];degree=ref 0};;
let degree e = !(e.degree);;
let add_link e id = 
	let rec loop l = match l with
	| [] -> incr e.degree;[id]
	| hd::tl when hd <> id -> hd::(loop tl)
	| _ -> l
	in
	e.link <- loop e.link;;
let are_linked e1 e2 =
	let rec loop l = match l with
	| [] -> false
	| hd::_ when hd=e2.id -> true
	| _::tl -> loop tl
	in
	loop e1.link
;;


type graph = graph_element array;;

(* Creates a graph with s nodes. Each node has an unique id and is unconnected. *)
let make s = Array.init s (fun x -> {id=x;link=[];degree=ref 0});;


Random.self_init();;
let random_but_not i j k n =
	let l = ref (Random.int n) in
	while !l=i || !l=j || !l=k do
		l := Random.int n
	done;
	!l
;;

(* Creates a random network with s nodes.
The graph will be connexe with a maximal degree = max_degree +- 2.
*)
let create_random_network s max_degree = 
	let g = make s in
	let prq = Heap.make (s+1) 0 in
	Heap.insert prq 0 0;
	Heap.insert prq 1 0;
	Heap.insert prq 2 0;
	for l = 3 to (s-1) do
	(*
	while (Heap.size prq) >= 3 do*)
		let i = Heap.pop prq in
		let j = Heap.pop prq in
		let k = Heap.pop prq in
		if degree g.(l) < max_degree then
		begin
			add_link g.(i) l;add_link g.(l) i;
			add_link g.(j) l;add_link g.(l) j;
			add_link g.(k) l;add_link g.(l) k;
		end;
		if degree g.(i) < max_degree then Heap.insert prq i (degree g.(i));
		if degree g.(j) < max_degree then Heap.insert prq j (degree g.(j));
		if degree g.(k) < max_degree then Heap.insert prq k (degree g.(k));
		if degree g.(l) < max_degree then Heap.insert prq l (degree g.(l));
	done;
	g
;;
