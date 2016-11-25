open Core.Std;;
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

(* Creates a graph from a matrix. *)
let graph_of_matrix m =
	let n = Array.length m in
	let g = make n in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			if m.(i).(j) then add_link g.(i) j
		done
	done;
	g
;;

(* Builds a random graph with the Watts and Strogatz method.
*)
let wattsStrogatz n k beta =
	let l = Array.init n 
	(fun i -> Array.init n (fun j -> if (j<i-k/2)||(j=i)||j>(i+k/2) then false else true)) in
	let wire i j = l.(i).(j) <- true;l.(j).(i) <- true in
	let unwire i j = l.(i).(j) <- false;l.(j).(i) <- false in
	let wired i j = l.(i).(j) in
	for i = 0 to n-2 do
		for j = i+1 to min (i+k/2) n-1 do
			let r = Random.float 1.0 in
			if r < beta then begin
				let k = ref (Random.int n) in
				while (wired i !k) || (!k = i) do
					k := Random.int n
				done;
				unwire i j;wire i !k
			end
		done;
	done;
	graph_of_matrix l
;;