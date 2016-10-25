type 'a priority_queue_element = {priority:int; element:'a};;

type 'a priority_queue = {size:int ref; queue:'a priority_queue_element array};;

let swap t i j = 
	let tmp = t.(i) in
	t.(i) <- t.(j);
	t.(j) <- tmp
;;

let make s e = 
	let t = Array.make s {priority=0;element=e} in
	{size=ref 0;queue=t}
;;

(* Creates a priority queue from an array.
The nth element will have a f(n) priority.
 *)
let from_array a f =
	let s = Array.length a in
	let t = Array.init s (fun x -> {priority=f x; element=a.(x)}) in
	{size=ref 0;queue=t}
;;

let size q = !(q.size);;
let empty q = q.size:=0;;
let is_empty q = !(q.size)=0;;
let priority q i = q.queue.(i).priority;;

let insert q e p =
	let t = q.queue in
	let size = size q in
	let k = ref (size+1) in
	t.(!k) <- {priority=p;element=e};
	while !k>1 && t.(!k/2).priority >= t.(!k).priority do
		swap t !k (!k/2);k := !k/2;
	done;
	q.size := size+1 
;;

let pop q =
	let t = q.queue in
	let size = size q in
	let rec loop k = 
		let l,r = 2*k,2*k+1 in
		let maxi = ref k in
		if l <= (size-1) && priority q l < priority q k then maxi := l;
		if r <= (size-1) && priority q r < priority q !maxi then maxi := r;
		if k <> !maxi then
		begin
			swap t k !maxi;loop !maxi
		end
	in
	swap t 1 size;loop 1;q.size:=size-1;t.(size).element
;;
