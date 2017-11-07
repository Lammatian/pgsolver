(* The recursive algorithm
 *
 * from:
 * Wieslaw Zielonka. Infinite Games on Finitely Coloured Graphs with Applications to Automata on Infinite Trees
 * Theor. Comput. Sci. 200(1-2): 135-183 (1998)
 *)

open Basics;;
open Paritygame;;
open Univsolve;;
open Pgnodeset;;
open Pgplayer;;
open Pgpriority;;
open Pgsolution;;



let solver rec_solve game =
    let msg_tagged v = message_autotagged v (fun _ -> "RECURSIVE") in
    let msg_plain = message in
    let l = game#size in
    msg_tagged 3 (fun _ -> "Solving the game: " ^ game#format_game ^ "\n");
    let solution = new array_solution l in
    let strategy = Array.make l (-1) in

    let max_prio = ref (-1) in
    let more_than_one = ref false in
    game#iterate (fun v -> fun (pr,_,_,_,_) -> if pr <> -1
					     then (if !max_prio > -1 && pr <> !max_prio then more_than_one := true;
						   if pr > !max_prio then max_prio := pr));

    if !max_prio = -1 then (solution, strategy)
    else if not !more_than_one
    then (message 3 (fun _ -> "  Only " ^ (if !max_prio mod 2 = 0 then "even" else "odd") ^
                     " priority " ^ string_of_int !max_prio ^ " present\n");
          let winner = plr_benefits !max_prio in
	  game#iterate (fun v -> fun (p',pl',ws,_,_) -> if p' <> -1
						      then (solution#set v winner;
							    if pl'=winner then strategy.(v) <- ns_some ws));
          message 3 (fun _ -> "  Returned solution:\n    " ^
                     solution#format ^ "\n");
          message 3 (fun _ -> "  Returned strategy:\n    " ^
                     format_strategy strategy ^ "\n");
          (solution,strategy))
    else (
    	  let max_prio = ref (game#get_max_prio) in
    	  let pl = plr_benefits !max_prio in
          msg_tagged 3 (fun _ -> "Highest priority " ^ string_of_int !max_prio ^ " belongs to player " ^
                              plr_show pl ^ "\n");
          let nodes_with_prio_p = game#collect_max_parity_nodes in

          msg_tagged 3 (fun _ -> "The following nodes have priority " ^ string_of_int !max_prio ^
                              ": P = {" ^ String.concat "," (List.map string_of_int (ns_nodes nodes_with_prio_p))
                              ^ "}\n");

          let attr = game#attr_closure_inplace strategy pl nodes_with_prio_p in

          msg_tagged 3 (fun _ -> "The attractor of P for player " ^ plr_show pl ^ " is: {" ^
                     String.concat "," (List.map string_of_int (ns_nodes attr)) ^ "}\n");

        (*
          let game' = game#copy in
          game'#remove_nodes attr;
          *)
            let (game', map_to_sub, map_to_game) = game#subgame_by_node_filter (fun i ->
                not (ns_elem i attr)
            ) in

          msg_tagged 3 (fun _ -> "First recursive descent to subgame ...\n");

          let (sol,str) = !rec_solve game' in

          msg_tagged 3 (fun _ -> "Checking whether the opponent wins from any node of the subgame ...");

          if not (sol#exists (fun _ s -> s = plr_opponent pl))
          then (msg_plain 3 (fun _ -> " no\n");
                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

        		game'#iterate (fun v (_,pl',_,_,_) ->
        		    solution#set (map_to_game v) pl;
                    if pl' = pl then strategy.(map_to_game v) <- map_to_game str.(v)
                );
                ns_iter (fun v -> solution#set v pl) attr;
                ns_iter (fun v -> if game#get_owner v = pl then let ws = game#get_successors v in strategy.(v) <- ns_some ws) nodes_with_prio_p;

                msg_tagged 3 (fun _ -> "Solution: " ^ solution#format ^ "\n");
                msg_tagged 3 (fun _ -> "Strategy: " ^ format_strategy strategy ^ "\n");
                (solution,strategy))
          else (message 3 (fun _ -> " yes\n");
                let opponent_win = ref ns_empty in
                let opp = plr_opponent pl in
                sol#iter (fun v s ->
                  if s = opp then opponent_win := ns_add (map_to_game v) !opponent_win
                );

                msg_tagged 3 (fun _ -> "Opponent " ^ plr_show opp ^ " wins from nodes Q = {" ^
                          String.concat "," (List.map string_of_int (ns_nodes !opponent_win)) ^ "}\n");

                ns_iter (fun v -> strategy.(v) <- -1) attr;

                let attr = game#attr_closure_inplace strategy opp !opponent_win in
                msg_tagged 3 (fun _ -> "The attractor of Q for player " ^ plr_show opp ^ " is: {" ^
                          String.concat "," (List.map string_of_int (ns_nodes attr)) ^ "}\n");

                ns_iter (fun v -> solution#set v opp) attr;
                ns_iter (fun v ->
                    solution#set v opp;
                    if game#get_owner v = opp
                    then strategy.(v) <- map_to_game str.(map_to_sub v)
                ) !opponent_win;

(*
                let game' = game#copy in
                game'#remove_nodes attr;

                msg_tagged 3 (fun _ -> "Second recursive descent to subgame ....\n");

                let (sol,str) = !rec_solve game' in

                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                game'#iterate (fun v -> fun (_,ow,_,_,_) -> solution.(v) <- sol.(v);
							 if ow = sol.(v) then strategy.(v) <- str.(v));
							 *)

                let (game', map_to_sub, map_to_game) = game#subgame_by_node_filter (fun i ->
                    not (ns_elem i attr)
                ) in

                msg_tagged 3 (fun _ -> "Second recursive descent to subgame ....\n");

                let (sol,str) = !rec_solve game' in

                msg_tagged 3 (fun _ -> "Merging and completing strategies and solutions:\n");

                game'#iterate (fun v -> fun (_,ow,_,_,_) -> solution#set (map_to_game v) (sol#get v);
							 if ow = sol#get v then strategy.(map_to_game v) <- map_to_game str.(v));


                msg_tagged 3 (fun _ -> "Solution: " ^ solution#format ^ "\n");
                msg_tagged 3 (fun _ -> "Strategy: " ^ format_strategy strategy ^ "\n");

                (solution,strategy)));;

let mcnaughton_zielonka game options =
	let f = ref (fun _ -> (new array_solution 0, [||])) in
	f := universal_solve (universal_options_alter_verb options verbosity_level_default) (solver f);
(*	f := solver f; *)
	universal_solve options (solver f) game;;

let solve2 recsolve = solver recsolve;;

let solve game =
  mcnaughton_zielonka game (universal_solve_init_options_verbose !universal_solve_global_options);;


let fallback_solve game backend options =
	let f = ref (fun _ -> (new array_solution 0, [||])) in
	f := universal_solve (universal_options_alter_verb options verbosity_level_default) (solver f);
	universal_solve_fallback options backend (solver f) game;;



let register _ = Solverregistry.register_solver solve "recursive" "re" "use the recursive algorithm due to McNaughton / Zielonka";;
