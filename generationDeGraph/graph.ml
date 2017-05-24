open Core.Std;;

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
	for i = 0 to n-1 do
		for j = i+1 to (i+k/2) do
			let r = Random.float 1.0 in
			if r < beta then begin
        unwire i j;
				let k = ref (Random.int n) in
				while (wired i !k) || (!k = i) do
					k := Random.int n
				done;
				wire i !k
			end
		done;
	done;
	l
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

let degree g i =
  let n = Array.length g in
  let r = ref 0 in
  for j = 0 to (n-1) do
     if g.(i).(j) then incr r
  done;
  !r
;;

let maxDegree g n =
  let deg = degree g in
  let size = Array.length g in
  let rec loop i r = if i >= size then r else
    loop (i+1) ((deg i, i)::r)
  in
  SortFirst.sortFirst n (loop 0 [])
;;

let maxBetweenness g n =
  let a = betweenness g in
  let size = Array.length g in
  let rec loop i r = if i >= size then r else
    loop (i+1) ((a.(i), i)::r)
  in
  SortFirst.sortFirst n (loop 0 [])
;;
