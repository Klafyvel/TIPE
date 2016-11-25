type 'a tree =
	| Nil
	| Node2 of 'a * 'a tree * 'a tree
	| Node3 of 'a * 'a * 'a tree * 'a tree * 'a tree
;;

let make () = Nil;; 

let rec find tree e = match tree with
	| Nil -> false
	| Node2(v,l,_) when v > e -> find l e
	| Node2(v,_,r) when v < e -> find r e
	| Node3(vl,_,l,_,_) when vl > e -> find l e 
	| Node3(vl,vr,_,m,_) when (vl < e) && (vr > e) -> find m e
	| Node3(_,vr,_,_,r) when vr < e -> find r e
	| _ -> true
;;

let rec min tree = match tree with 
  | Nil -> failwith "min"
  | Node2(v,Nil,_) -> v
  | Node3(v,_,Nil,_,_) -> v
  | Node2(_,n,_) -> min n 
  | Node3(_,_,n,_,_) -> min n
;;

let rec max tree = match tree with 
  | Nil -> failwith "max"
  | Node2(v,_,Nil) -> v
  | Node3(_,v,_,_,Nil) -> v
  | Node2(_,_,n) -> max n 
  | Node3(_,_,_,_,n) -> max n
;;

let insert t e = 
  let spread v1 v2 v3 t1 t2 t3 t4 = Node2(v2,Node2(v1,t1,t2),Node2(v3,t3,t4)) in
  let node n = match n with 
  | Node2(v,l,r) -> v,l,r 
  | _ -> failwith "Node 2"
  in
  let rec loop t = match t with
  | Nil -> Node2(e,Nil,Nil),true
  | Node2(v,l,r) when e=v -> Node2(v,l,r), false
  | Node3(v1,v2,l,m,r) when (e = v1) || (e = v2) -> Node3(v1,v2,l,m,r), false
  | Node2(v,l,r) when e < v -> let res,s = loop l in
    if s then
      let v1,t1,t2 = node res in
      Node3(v1,v,t1,t2,r), false
    else
      Node2(v,res,r), false
  | Node2(v,l,r) when e > v -> let res,s = loop r in
    if s then
      let v1,t1,t2 = node res in
      Node3(v,v1,l,t1,t2), false
    else
      Node2(v,l,res), false
  | Node3(vl,vr,l,m,r) when e < vl -> let res,s = loop l in 
    if s then 
      let v,t1,t2 = node res in
      (spread v vl vr t1 t2 m r), true
    else
      Node3(vl,vr,res,m,r), false
  | Node3(vl,vr,l,m,r) when (e >= vl) && (e < vr) -> let res, s = loop m in
    if s then
      let v,t1,t2 = node res in
      (spread vl v vr l t1 t2 r), true
    else
      Node3(vl,vr,l,res,r), false
  | Node3(vl,vr,l,m,r) when (e >= vr) -> let res, s = loop r in
    if s then
      let v,t1,t2 = node res in
      (spread vl vr v l r t1 t2), true
    else
      Node3(vl,vr,l,m,res), false
  | _ -> failwith "Unmatched"
  in
  fst (loop t)
;;

let remove t e = 
	let unpack v = match v with
	| Some a -> a
	| None -> failwith "unpack"
	in
	let merge_right n = match n with
	| Node2(v,Node3(vll,vlr,ll,lm,lr), Node2(_,rl,_)) -> Node2(vlr,Node2(vll,ll,lm),Node2(v,lr,rl)), false
	| Node3(vl,vr,node_left,Node3(vll,vlr,ll,lm,lr),Node2(_,rl,_)) -> Node3(vl,vlr,node_left,Node2(vll,ll,lm),Node2(vr,lr,rl)), false
	| Node3(vl,vr,Node3(vll,vlr,ll,lm,lr),Node2(vm,ml,mr),Node2(_,rl,_)) -> Node3(vlr,vm,Node2(vll,ll,lm), Node2(vl,lr,ml),Node2(vr,mr,rl)), false
	| Node3(vl,vr,Node2(vll,ll,lr),Node2(vmm,ml,mr),Node2(_,rl,_)) -> Node2(vmm,Node3(vll,vl,ll,lr,ml),Node2(vr,mr,rl)), false
	| Node2(v,Node2(vl,l,r),Node2(_,rl,_)) -> Node2(v,Node3(vl,v,l,r,rl),Nil), true
	| _ -> failwith "merge right"
	in
	let merge_left n = match n with
	| Node2(v,Node2(_,ll,_), Node3(vrl,vrr,rl,rm,rr)) -> Node2(vrl,Node2(v,ll,rl),Node2(vrr,rm,rr)), false
	| Node3(vl,vr,Node2(_,ll,_),Node3(vml,vmr,ml,mm,mr),right_node) -> Node3(vml,vr,Node2(vl,ll,ml),Node2(vmr,mm,mr),right_node), false
	| Node3(vl,vr,Node2(_,ll,_),Node2(vm,ml,mr),Node3(vrl,vrr,rl,rm,rr)) -> Node3(vm,vrl,Node2(vl,ll,ml), Node2(vr,mr,rl),Node2(vrr,rm,rr)), false
	| Node3(vl,vr,Node2(_,ll,_),Node2(vmm,ml,mr),Node2(vrr,rl,rr)) -> Node2(vmm,Node2(vl,ll,ml),Node3(vr,vrr,mr,rl,rr)), false
	| Node2(v,Node2(_,ll,_),Node2(vr,l,r)) -> Node2(v,Node3(v,vr,ll,l,r),Nil), true
	| _ -> failwith "merge left"
	in
	let merge_middle n = match n with
	| Node3(vl,vr,Node3(vll,vlr,ll,mm,rr),Node2(_,ml,_),right_node) -> Node3(vlr,vr,Node2(vll,ll,mm),Node2(vl,rr,ml),right_node), false
	| Node3(vl,vr,left_node,Node2(_,ml,_),Node3(vrl,vrr,ll,mm,rr)) -> Node3(vl,vrl,left_node,Node2(vr,ml,ll),Node2(vrr,mm,rr)), false
	| _ -> failwith "merge middle"
	in
	let rec loop ?(min=false) t = match t with
	| Nil -> failwith "Not found"
	| Node2(v,Nil,Nil) when min -> Some v,Node2(v,Nil,Nil),true
	| Node3(v,r,Nil,Nil,Nil) when min || v=e -> Some v,Node2(r,Nil,Nil),false
	| Node2(v,Nil,Nil) when v=e -> None,Node2(v,Nil,Nil),true
	| Node3(l,v,Nil,Nil,Nil) when v=e -> None,Node2(l,Nil,Nil),false
	| Node2(v,l,r) when e = v -> 
	let n_val,n_r,underflow = loop ~min:true r in
	if underflow then
		let n_node,underflow = merge_right (Node2(unpack n_val,l,n_r)) in
		None,n_node,underflow
	else
		None,Node2(unpack n_val,l,n_r),false
	| Node3(vl,vr,l,m,r) when e = vl -> 
	let n_val,n_m,underflow = loop ~min:true m in
	if underflow then
		let n_node,underflow = merge_middle (Node3(unpack n_val,vr,l,n_m,r)) in
		None,n_node,underflow
	else
		None,Node3(unpack n_val,vr,l,n_m,r),false
	| Node3(vl,vr,l,m,r) when e = vr ->
	let n_val,n_r,underflow = loop ~min:true r in
	if underflow then
		let n_node,underflow = merge_right (Node3(vl,unpack n_val,l,m,n_r)) in
		None,n_node,underflow
	else
		None,Node3(vl,unpack n_val,l,m,n_r),false
	| Node2(v,l,r) when e < v || min -> 
	let v_option,n_l,underflow = loop ~min:min l in
	if underflow then
		let n_node,underflow = merge_left (Node2(v,n_l,r)) in
		v_option,n_node,underflow
	else
		v_option,Node2(v,n_l,r),false
	| Node2(v,l,r) when e > v -> 
	let v_option,n_r,underflow = loop r in
	if underflow then
		let n_node,underflow = merge_right (Node2(v,l,n_r)) in
		v_option,n_node,underflow
	else
		v_option,Node2(v,l,n_r),false
	| Node3(vl,vr,l,m,r) when e < vl || min->
	let v_option,n_l,underflow = loop ~min:min l in
	if underflow then
		let n_node,underflow = merge_left (Node3(vl,vr,n_l,m,r)) in
		v_option,n_node,underflow
	else
		v_option,Node3(vl,vr,n_l,m,r),false
	| Node3(vl,vr,l,m,r) when e > vl && e < vr ->
	let v_option,n_m,underflow = loop m in
	if underflow then
		let n_node,underflow = merge_middle (Node3(vl,vr,l,n_m,r)) in
		v_option,n_node,underflow
	else
		v_option,Node3(vl,vr,l,n_m,r),false
	| Node3(vl,vr,l,m,r) when e > vr ->
	let v_option,n_r,underflow = loop r in
	if underflow then
		let n_node,underflow = merge_middle (Node3(vl,vr,l,m,n_r)) in
		v_option,n_node,underflow
	else
		v_option,Node3(vl,vr,l,m,n_r),false
	| _ -> failwith "Remove"
	in
	let _,res,underflow = loop t in
	if underflow then (
	match res with 
	| Node2(_,l,_) -> l
	| _ -> failwith "Remove"
	)
	else
	res
;;