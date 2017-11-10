open Basics ;;
open Paritygame;;
open Arrayparitygame;;
open Univsolve;;
open Transformations;;
open Tcsarray;;
open Tcslist;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Pgnode;;
open Pgstrategy;;


let array_max a less = ArrayUtils.max_elt (fun x y -> if less x y then -1 else 1) a
let list_max a less = ListUtils.max_elt (fun x y -> if less x y then -1 else 1) a


let msg_tagged v = message_autotagged_newline v (fun _ -> "OPTSTRATIMPR");;
let msg_plain = message;;


type estentry = PosInfty | Escape of int array;;
type estimation = estentry array;;


let compare est1 = function
	PosInfty -> (match est1 with PosInfty -> 0 | _ -> -1)
|	Escape esc2 -> match est1 with
        PosInfty -> 1
    |   Escape esc1 ->
            let rec compare'' = function
                0 -> 0
            |   j -> let c = compare esc1.(j-1) esc2.(j-1) in
                     if c = 0 then compare'' (j-1)
                     else if j mod 2 = 0 then c else -c
            in
                compare'' (Array.length esc1)

let add_est est pr =
	match est with
		Escape esc ->
            let esc' = Array.copy esc in
            if pr > 0 then esc'.(pr - 1) <- esc'.(pr - 1) + 1;
            Escape esc'
    |	other -> other;;

let initial_estimation game d =
	let n = game#size in
	let rew = reward plr_Even in
	Array.init n (fun i -> let pl = game#get_owner i in
			       let delta = game#get_successors i in
			       let e = Array.make d 0 in
			       if pl = plr_Odd then (
				 let pr = ns_fold (fun pr j -> let pr' = game#get_priority j in
							       if rew pr' > rew pr	then pr	else pr'
						  ) (2 * d) delta in
				 if pr > 0 then e.(pr - 1) <- 1
			       );
			       Escape e
	);;

let format_estentry = function PosInfty -> "+oo" | Escape esc ->
	ListUtils.format (fun (i, v) -> string_of_int i ^ ":" ^ string_of_int v) (List.filter (fun (_, v) -> v != 0) (Array.to_list (Array.mapi (fun i v -> (i + 1, v)) esc)))

let improvement_arena (game: paritygame) estimation =
	game#subgame_by_edge_pred (fun u v ->
		compare estimation.(u) (add_est estimation.(v) (game#get_priority v)) <= 0
	);;

let zero_arena arena estimation =
	let pred u v = compare estimation.(u) (add_est estimation.(v) (arena#get_priority v)) = 0 in
	arena#subgame_by_edge_pred (fun u v -> let pl = arena#get_owner u in
					       let delta = arena#get_successors u in
					       (pred u v) && ((pl = plr_Odd) || (ns_forall (pred u) delta))
	);;

let is_infty = function PosInfty -> true | _ -> false;;

let is_zero = function Escape esc -> ArrayUtils.forall esc (fun _ e -> e = 0) | _ -> false;;

let counter_strategy zero_arena estimation =
	let seed = zero_arena#collect_nodes (fun i (_, pl, delta, _, _) -> pl = plr_Even && ns_size delta = 0 && (not (is_infty estimation.(i)))) in
	let strat = new array_strategy (zero_arena#size) in
	let _ = zero_arena#attr_closure_inplace strat plr_Odd seed in
	strat;;

let addition d = function
 	(PosInfty, _) -> PosInfty
|	(_, PosInfty) -> PosInfty
|	(Escape a, Escape b) -> Escape (Array.mapi (fun i x -> x + b.(i)) a);;

let subtraction d = function
	(PosInfty, PosInfty) -> Escape (Array.make d 0)
|	(PosInfty, _) -> PosInfty
|	(_, PosInfty) -> failwith "neg infty"
|	(Escape a, Escape b) -> Escape (Array.mapi (fun i x -> x - b.(i)) a);;

let improv_pot game d (u, v) est =
    let est' = add_est est.(v) (game#get_priority v) in
    subtraction d (est', est.(u))

let minop ests = if Array.length ests = 0 then PosInfty else array_max ests (fun x y -> compare x y > 0);;

let maxop ests = array_max ests (fun x y -> compare x y < 0);;

let format_estimation arr =
	let s = ref "" in
	for i = 0 to Array.length arr - 1 do
		s := !s ^ string_of_int i ^ " : " ^ (format_estentry arr.(i)) ^ "\n"
	done;
	!s;;

let basic_update_step arena d estimation =
    msg_tagged 3 (fun _ -> "\nEstimation:\n" ^ format_estimation estimation ^ "\n");
    msg_tagged 3 (fun _ -> "Arena:\n" ^ arena#to_string ^ "\n");
	let n = arena#size in
	let upd = Array.make n PosInfty in
	(* 0 = not evaluated, 1 = evaluated, 2 = all_eval, 3 = red_one0_eval *)
	let eval_state = Array.make n 0 in
	let todo = ref n in
	let red_all_evaluated = ref [] in
	let red_one0_evaluated = ref [] in
	let green_all_evaluated = ref [] in
	let red_rest = ref [] in

	let dequeue l =
		let h = List.hd !l in
		l := List.tl !l;
		h
	in

        arena#iterate (fun i -> fun (_,pl,delta,_,_) -> if ns_size delta = 0 then (
						       if pl = plr_Even
						       then green_all_evaluated := i::!green_all_evaluated
						       else red_all_evaluated := i::!red_all_evaluated;
						       eval_state.(i) <- 2
						     )
						     else if pl = plr_Odd then red_rest := i::!red_rest);
			     
	let update_todo i =
		eval_state.(i) <- 1;
		decr todo;
		ns_iter (fun j ->
			   let pl = arena#get_owner j in
			   let delta = arena#get_successors j in
			   if (eval_state.(j) = 0) || (eval_state.(j) = 3) then (
			     if ns_forall (fun k -> eval_state.(k) = 1) delta then (
                	       if pl = plr_Even
			       then green_all_evaluated := j::!green_all_evaluated
			       else red_all_evaluated := j::!red_all_evaluated;
			       eval_state.(j) <- 2;
			     )
			     else if (pl = plr_Odd) then (
                	       if (ns_exists (fun k -> (eval_state.(k) = 1) && (is_zero upd.(k)) && (is_zero (improv_pot arena d (j, k) estimation))) delta)
                	       then (
                		 red_one0_evaluated := j::!red_one0_evaluated;
                		 eval_state.(j) <- 3;
                	       )
			     )
			   )
			  ) (arena#get_predecessors i)
	in

	while !todo > 0 do
		if not (!red_all_evaluated = []) then (
			let r = dequeue red_all_evaluated in
			if eval_state.(r) = 2 then (
				msg_tagged 3 (fun _ -> "Rule 1 is applied to " ^ string_of_int r ^ " : ");
                update_todo r;
                let delta = Array.of_list (ns_nodes (arena#get_successors r)) in
                upd.(r) <- minop (Array.map (fun g -> addition d (upd.(g), (improv_pot arena d (r, g) estimation))) delta);
                msg_plain 3 (fun _ -> format_estentry upd.(r) ^ "\n");
            )
		)
		else if not (!red_one0_evaluated = []) then (
			let r = dequeue red_one0_evaluated in
			if eval_state.(r) = 3 then (
				msg_tagged 3 (fun _ -> "Rule 2 is applied to " ^ string_of_int r ^ " : ");
                update_todo r;
                upd.(r) <- Escape (Array.make d 0);
                msg_plain 3 (fun _ -> format_estentry upd.(r) ^ "\n");
            )
		)
		else if not (!green_all_evaluated = []) then (
			let r = dequeue green_all_evaluated in
			if eval_state.(r) = 2 then (
				msg_tagged 3 (fun _ -> "Rule 3 is applied to " ^ string_of_int r ^ " : ");
                update_todo r;
                let delta =  Array.of_list (ns_nodes (arena#get_successors r)) in
                if Array.length delta = 0
                then upd.(r) <- Escape (Array.make d 0)
                else upd.(r) <- maxop (Array.map (fun g -> addition d (upd.(g), (improv_pot arena d (r, g) estimation))) delta);
                msg_plain 3 (fun _ -> format_estentry upd.(r) ^ "\n");
            )
		)
		else (
			red_rest := List.filter (fun r -> eval_state.(r) = 0) !red_rest;
			let (i, est) = List.fold_left (fun (i, est) r ->
						       let delta =  Array.of_list (ns_nodes (arena#get_successors r)) in
						       let est' = minop (Array.map (fun g ->
										    if eval_state.(g) = 1
										    then addition d (upd.(g), (improv_pot arena d (r, g) estimation))
										    else PosInfty
										   ) delta) in
						       if (i = -1) || (compare est est' >= 0)
						       then (r, est')
						       else (i, est)
						      ) (-1, PosInfty) !red_rest in
			msg_tagged 3 (fun _ -> "Rule 4 is applied to " ^ string_of_int i ^ " : ");
			update_todo i;
			upd.(i) <- est;
			msg_plain 3 (fun _ -> format_estentry upd.(i) ^ "\n");
		)
 	done;

 	Array.mapi (fun i x -> addition d (x, upd.(i))) estimation;;



let update_strategy0 arena est_after strat =
	Array.iteri (fun i est ->
		     let pl = arena#get_owner i in
		     let delta =  Array.of_list (ns_nodes (arena#get_successors i)) in
		     if (pl = plr_Even) && (strat#get i = nd_undef) && (is_infty est)
		     then strat#set i (array_max delta (fun x y -> compare est_after.(x) est_after.(y) < 0))
		    ) est_after;;
  

let get_intermediate_strategy0 arena est_after strat' =
	let strat = strat'#copy in
	Array.iteri (fun i est ->
		let delta =  Array.of_list (ns_nodes (arena#get_successors i)) in
		if (arena#get_owner i = plr_Even) && (strat#get i = nd_undef) && (Array.length delta > 0)
		then strat#set i (array_max delta (fun x y -> compare (add_est est_after.(x) (arena#get_priority x))
		                                                      (add_est est_after.(y) (arena#get_priority y)) < 0))
	) est_after;
	strat


let solve_scc game' =
	let game = alternating_transformation game' false in

	let n = game#size in
	msg_tagged 3 (fun _ -> "Entering SCC of size " ^ string_of_int n ^ "\n");
	let d = game#get_max_prio in
	let est = ref (initial_estimation game d) in
	let eq = ref false in
	let strat = new array_strategy n in
	let arena = ref (new array_pg 0) in
	let counter = ref 0 in
	while not (!eq) do
        incr counter;
        msg_tagged 2 (fun _ -> "Iteration: " ^ string_of_int !counter ^ "\r");
		arena := improvement_arena game !est;
		let est' = basic_update_step !arena d !est in
		update_strategy0 !arena est' strat;
        msg_tagged 3 (fun _ -> "\nIntermediate strategy: " ^ (get_intermediate_strategy0 !arena est' strat)#format ^ "\n");
		eq := ArrayUtils.forall est' (fun i entry -> compare (!est).(i) entry = 0);
		est := est';
	done;
	if !verbosity = 2 then msg_plain 2 (fun _ -> "\n");
	let sol = sol_init game (fun i -> if is_infty (!est).(i) then plr_Even else plr_Odd) in

	let strat' = counter_strategy (zero_arena !arena !est) !est in
	strat#merge_inplace strat';

	alternating_revertive_restriction game' game sol strat;;


let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve_scc game;;

let register _ =
    Solverregistry.register_solver solve "optstratimprov" "os" "use the optimal strategy impr. algorithm due to Schewe";;
