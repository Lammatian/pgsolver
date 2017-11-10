open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Tcslist;;
open Tcsstrings;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;



let improvement_policy_optimize_roundrobin game node_total_ordering (e, next) old_strategy valu =
	let strategy = old_strategy#copy in
	let e = ref e in
	let fnd = ref false in
	while (not !fnd) do
		let (v,w) = !e in
		if node_valuation_ordering game node_total_ordering valu.(old_strategy#get v) valu.(w) < 0 then (
			strategy#set v w;
			fnd := true
		)
		else
			e := next !e;
	done;
	(strategy, (!e, next))
	
let improvement_policy_optimize_roundrobin_default_first_edge game v =
	let n = game#size in
	let i = ref ((v+1) mod n) in
	while (game#get_owner !i = plr_Odd) do
		i := (!i + 1) mod n
	done;
	(!i, ns_some (game#get_successors !i))

let improvement_policy_optimize_roundrobin_default_next game (v,w) =
	let tr = Array.of_list (ns_nodes (game#get_successors v)) in
	let idx = ref 0 in
	while (tr.(!idx) <> w) do
		incr idx
	done;
	if !idx + 1 < Array.length tr then (v,tr.(!idx+1))
	else improvement_policy_optimize_roundrobin_default_first_edge game v
	
let improvement_policy_optimize_roundrobin_ordering_next game (ordering, backtransform) (v,w) =
	backtransform.(((TreeMap.find (v,w) ordering)+1) mod (Array.length backtransform))
		
let improvement_policy_optimize_roundrobin_ordering_first game (ordering, backtransform) =
	backtransform.(0)

let improvement_policy_optimize_roundrobin_ordering_build game cmp =
	let l = ref [] in
	game#iterate (fun v (_, pl, tr, _, _) ->
		if (pl = plr_Even) then
			ns_iter (fun w ->
				l := (v,w)::!l
			) tr
	) ;
	let a = Array.of_list !l in
	Array.sort cmp a;
	let m = ref (TreeMap.empty compare) in
	Array.iteri (fun i e ->
		m := TreeMap.add e i !m
	) a;
	(!m, a)

let lower_bound_construction_round_robin_compare game (v,w) (v',w') =
	let f i =
		match game#get_desc i with
			None -> ('!', 0, 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, -1, -1)
				else if String.get s 1 = '('
				then let t = StringUtils.explode (StringUtils.rest_string s 2) ',' in
 				     let x = int_of_string (List.hd t) in
					 let y = int_of_string (List.hd (StringUtils.explode (List.hd (List.tl t)) ')')) in
					 (String.get s 0, x, y)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1), -1)
	in
	let g = function
		('d','e') -> 0
	|	('d','d') -> 0
	|	('u',_) -> 1
	|	('d','p') -> 2
	|	('d','u') -> 2
	|	('w',_) -> 3
	|	_ -> -1
	in
		let (vv,vi,vij) = f v in
		let (ww,_,_) = f w in
		let (vv',vi',vij') = f v' in
		let (ww',_,_) = f w' in
		let vx = g (vv,ww) in
		let vx' = g (vv',ww') in
		let (c,ci,cij) = (compare vx vx', compare vi vi', compare vij vij') in
		if c != 0 then c
		else match vx with
				0 -> if ci = 0 then cij else ci
			|	1 -> -ci
			|	2 -> -cij
			|	3 -> -ci
			|	_ -> 0

let lower_bound_construction_round_robin_compare_exp game (v,w) (v',w') =
	let f i =
		match game#get_desc i with
			None -> ('!', 0, 0)
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, -1, -1)
				else if String.get s 1 = '('
				then let t = StringUtils.explode (StringUtils.rest_string s 2) ',' in
 				     let x = int_of_string (List.hd t) in
					 let y = int_of_string (List.hd (StringUtils.explode (List.hd (List.tl t)) ')')) in
					 (String.get s 0, x, y)
				else (String.get s 0, int_of_string (StringUtils.rest_string s 1), -1)
	in
	let g = function		
		('e','x') -> 1   (* complex ordering ... *)
	|	('b', _ ) -> 1	 (* complex ordering ... *)
	|	('d','b') -> 1   (* complex ordering ... *)
	|	('c', _ ) -> 2   (* ordering irrelevant *)
	|	('e','p') -> 3   (* ordering irrelevant *)
	|	('d','x') -> 4   (* ordering irrelevant *)
	|	('a', _ ) -> 5   (* top-down ordering *)
	|	_ -> -1
	in
	let h = function
		('e',_) -> 2
	|	('b',_) -> 0
	|	('d',_) -> 1
	|	_ -> -1
	in
		let (vv,vi,vij) = f v in
		let (ww,_,_) = f w in
		let (vv',vi',vij') = f v' in
		let (ww',_,_) = f w' in
		let vx = g (vv,ww) in
		let vx' = g (vv',ww') in
		let (c,ci,cij) = (compare vx vx', compare vi vi', compare vij vij') in
		let h_cmp = compare (h (vv,ww)) (h (vv',ww')) in
		if c != 0 then c
		else match vx with
				5 -> -ci
			|	1 -> if ci != 0
				     then ci
				     else h_cmp
			|	_ -> 0




let strategy_improvement_optimize_roundrobin_policy game =
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	improvement_policy_optimize_roundrobin
	(improvement_policy_optimize_roundrobin_default_first_edge game (-1),
	 improvement_policy_optimize_roundrobin_default_next game) false "STRIMPR_ROUNDROBIN";;


let strategy_improvement_optimize_roundrobin_policy_lower_bound game =
	let lookup_node s =
        let check i =
            match (game#get_desc i) with
                None -> false
            |   Some t -> s=t
        in
        let i = ref 0 in
        let n = game#size  in
        while (!i < n) && (not (check !i)) do
            incr i
        done;
        if !i < n then Some !i else None
    in
    let is_exp = lookup_node "e0" != None in
	let cmp = if is_exp
	          then lower_bound_construction_round_robin_compare_exp game
	          else lower_bound_construction_round_robin_compare game
	in
	let struc = improvement_policy_optimize_roundrobin_ordering_build game cmp in
	strategy_improvement game initial_strategy_by_best_reward node_total_ordering_by_position
	improvement_policy_optimize_roundrobin
	(improvement_policy_optimize_roundrobin_ordering_first game struc,
	 improvement_policy_optimize_roundrobin_ordering_next game struc) false "STRIMPR_ROUNDROBIN";;



let register _ =
    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_roundrobin_policy g)
        "switchroundrob" "sirr" "Round Robin policy iteration";

    register_sub_solver
        (fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_optimize_roundrobin_policy_lower_bound g)
        "switchroundrobse" "sirrse" "Round Robin policy iteration with lower bound ordering";;

