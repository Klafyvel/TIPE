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

Random.self_init ();;
let wattsStrogatzMatrix n k beta =
	let l = Array.make_matrix n n false in
	let rec wire i j = if i < 0 then wire (n+i) j
    else if i >= n then wire (i-n) j
    else if j < 0 then wire i (n+j)
    else if j >= n then wire i (j-n)
    else (l.(i).(j) <- true;l.(j).(i) <- true) 
  in
	let rec unwire i j = if i < 0 then unwire (n+i) j
    else if i >= n then unwire (i-n) j
    else if j < 0 then unwire i (n+j)
    else if j >= n then unwire i (j-n)
    else (l.(i).(j) <- false;l.(j).(i) <- false)
  in
	let rec wired i j = if i < 0 then wired (n+i) j
    else if i >= n then wired (i-n) j
    else if j < 0 then wired i (n+j)
    else if j >= n then wired i (j-n)
    else l.(i).(j) 
  in
  for i=0 to n-1 do
    for j = i-k/2 to i+k/2 do
      if j != i then wire i j 
    done
  done;
	(*for i = 0 to n-1 do
		for j = i+1 to (i+k/2) do
			let r = Random.float 1.0 in
			if r < beta then begin
				let k = ref (Random.int n) in
				while (wired i !k) || (!k = i) do
					k := Random.int n
				done;
				unwire i j;wire i !k
			end
		done;
	done;*)
	l
;;
let wattsStrogatz n k beta =
	graph_of_matrix (wattsStrogatzMatrix n k beta)
;;

(* Betweenness centrality of a graph via its adjacency matrix*)
let betweenness g =
	let n = Array.length g in
  let cB = Array.create n (0.0) in
  for s = 0 to n-1 do
    let stack = Stack.create() in
    let p = Array.create n ([]) in
    let sigma = Array.create n ( 0.0) in
    sigma.(s) <- 1.0;
    let d = Array.create n ((-1)) in
    d.(s) <- 0;
    let q = Queue.create() in
    Queue.enqueue q s;
    while not (Queue.is_empty q) do
      let v = Queue.dequeue_exn q in
      Stack.push stack v;
      for w = 0 to (n-1) do
        let iw = g.(v).(w) in
        if iw then
          if d.(w) < 0 then begin
            Queue.enqueue q w;
            d.(w) <- d.(v) + 1;
          end;
          if d.(w) = (d.(v) + 1) then begin
            sigma.(w) <- sigma.(w) +. sigma.(v);
            p.(w) <- v::p.(w);
          end;
      done;
    done;
    let delta = Array.create n (0.0) in
    while not (Stack.is_empty stack) do
      let w = Stack.pop_exn stack in
      List.iter ~f:(fun v -> delta.(v) <- delta.(v) +. sigma.(v) /. sigma.(w) *. (1.0 +. delta.(w))) p.(w);
      if w != s then begin cB.(w) <- cB.(w) +. delta.(w); end;
    done;
  done;
  cB
;;

