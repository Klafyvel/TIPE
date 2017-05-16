open Core.Std;;


(*
  Spread the rumor and return the amount of nodes aware of it.
*)
let step_p graph a b s =
  let n = Array.length graph in
  let p_lim = b /. (a +. b) in 
  let nb = ref 0 in
  for i = 0 to n-1 do
    if not s.(i) then begin
      let aware = ref 0 in
      let d = ref 0 in
      for j = 0 to n-1 do
        if graph.(i).(j) then(
          incr d;
          if s.(j) then incr aware;
        )
      done;
      let p = (float_of_int !aware) /. (float_of_int !d) in
      if p > p_lim then (s.(i) <- true; incr nb)
    end
    else incr nb
  done;
  !nb
;;
  