open Arg ;;
open Tcsargs;;
open Tcsbasedata;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Str ;;
open Stratimpralgs;;
open Tcsset;;
open Pgnodeset;;
open Pgplayer;;
open Pgstrategy;;


let out s =
	print_string s;
	flush stdout

let _ =
	let header = Info.get_title "Parity Game Improvement Arena Tool" in
	  
	SimpleArgs.parsedef [] (fun _ -> ()) (header ^ "Usage: fullimprarena\n" ^
                                           "Provides some information about a parity game improvement arena.");

	let (in_channel,name) = (stdin,"STDIN") in

	let game = Parsers.parse_parity_game in_channel in
	
	let print_strat strategy =
		let valu = evaluate_strategy game node_total_ordering_by_position strategy in
		
		let compare_by_desc i j =
			compare (game#get_desc  i) (game#get_desc  j)
		in
		
		let ordered = ref (TreeSet.empty compare_by_desc) in
		let longest = ref 0 in
		
		game#iterate (fun i -> fun (_,_,_,_,desc) -> longest := max !longest (String.length (OptionUtils.get_some desc));
							   ordered := TreeSet.add i !ordered) ;
		
		let getd i = (StringUtils.fillup (OptionUtils.get_some (game#get_desc  i)) !longest ' ') in
		
		TreeSet.iter (fun i ->
			out (getd i);
			out " | ";
			let pl = game#get_owner  i in
			let tr = game#get_successors  i in 
			let j =
			  if pl = plr_Even then strategy#get i
			  else best_decision_by_valuation_ordering game node_total_ordering_by_position valu i
			in
			out (getd j);
			out " | ";
			if pl = plr_Even then (
				ns_iter (fun j ->
					if node_valuation_ordering game node_total_ordering_by_position valu.(strategy#get i) valu.(j) < 0
					then out (getd j ^ " ");
				) tr;
			);
			out "\n";
		) !ordered;
		out "\n\n";
	in
	
	let rec iterate_strat strategy i =
		if i >= game#size 
		then print_strat strategy
		else (
		let pl = game#get_owner  i in
		let tr = game#get_successors  i in 
		if pl = plr_Odd
		then iterate_strat strategy (i + 1)
		else ns_iter (fun j ->
			      strategy#set i j;
			      iterate_strat strategy (i + 1)
			     ) tr
		)
	in
		
	let strategy = new array_strategy game#size in
	
	iterate_strat strategy 0;

	out "\n";;
