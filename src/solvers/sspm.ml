open Basics;;
open Paritygame;;
open Tcsqueue;;

let log_debug msg = message_autotagged 3 (fun _ -> "SSPM") (fun _ -> msg ^ "\n") ;;
let log_verb msg = message_autotagged 2 (fun _ -> "SSPM") (fun _ -> msg ^ "\n") ;;
let log_info msg = message_autotagged 2 (fun _ -> "SSPM") (fun _ -> msg ^ "\n") ;;

let log2 x = log10 x /. log10 2.
(*let clog2 x = float_of_int x |> log2 |> ceil |> int_of_float*)
let clog2 x =
  if x = 0 then 0 else
  let rec find y p = if y >= x then p else find (y lsl 1) (p + 1) in
  find 1 0

let binary_length x = clog2 (x + 1)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
let pow2 n = 1 lsl n

module BString = struct
  type t = BString of int * int | Empty

  let set_mu m = ()

  let create x y =
    if y = 0 then Empty else BString (x, y)

  let create_len y = create 0 y

  let show = function
    | Empty -> "ε"
    | BString (x, y) ->
      let leading_zeros = String.make (y - binary_length x) '0' in
      let rec convert x s =
        if x > 0 then convert (x / 2) s ^ (x mod 2 |> string_of_int) else ""
      in
      leading_zeros ^ convert x ""

  let print bstr = show bstr |> print_string

  let length = function
    | Empty -> 0
    | BString (x, y) -> y

  let extend bstr n =
    if length bstr < n then
      match bstr with
        | Empty -> BString (pow2 (n - 1), n)
        | BString (x, y) ->
          BString (x lsl (n - y) + pow2 (n - y - 1), n)
    else
      bstr
  
  let cut = function
    | Empty -> Empty
    | BString (x, y) ->
      let rec cut_ones x y =
        if y = 0 then Empty
        else if x mod 2 = 1 then cut_ones (x asr 1) (y - 1) 
        else if y = 1 then Empty
        else BString (x asr 1, y - 1)
      in
      cut_ones x y

  let is_max = function
    | Empty -> false
    | BString (x, y) ->
      let rec loop x y =
        if x mod 2 = 0 then false
        else if x = 1 then y = 1
        else loop (x asr 1) (y - 1)
      in
      loop x y
  
  let is_empty = function
    | Empty -> true
    | BString _ -> false

  let compare b1 b2 =
    (* log_debug ("Comparing " ^ show b1 ^ " and " ^ show b2); *)
    match b1, b2 with
    | Empty, Empty          -> 0
    | Empty, BString (x, y) -> if binary_length x = y then -1 else 1
    | BString (x, y), Empty -> if binary_length x = y then 1 else -1
    | BString (x1, y1), BString (x2, y2) ->
      let zeros1 = y1 - binary_length x1 in
      let zeros2 = y2 - binary_length x2 in
      if zeros1 < zeros2 then 1
      else if zeros1 > zeros2 then -1
      else if x1 = x2 then 0
      else
      begin
        let cut_len = min (binary_length x1) (binary_length x2) in
        let cut_x1 = x1 asr (binary_length x1 - cut_len) in
        let cut_x2 = x2 asr (binary_length x2 - cut_len) in
        if cut_x1 > cut_x2 then 1
        else if cut_x1 < cut_x2 then -1
        else if cut_x1 = x1 then
          let check = pow2 (binary_length x2 - cut_len - 1) in
          if x2 land check <> 0 then -1 else 1
        else
          let check = pow2 (binary_length x1 - cut_len - 1) in
          if x1 land check <> 0 then 1 else -1
      end
end

module BString2 = struct
  type t = int * int

  (** Maximum length in bits **)
  let mlen = ref 0

  let set_mu m = mlen := m

  let create x y = (x, y)
  
  let create_len l = (pow2 (!mlen - l), l)

  let is_empty (b, l) = b = (pow2 !mlen)

  (** Max <=> all ones, not really a good name **)
  let is_max (b, l) = 
    let rec is_max_rec b l s p =
      if l = 0 then false
      else if b = s - p then true
      else is_max_rec b (l-1) s (2*p)
    in
    is_max_rec b !mlen (pow2 (!mlen + 1)) 1

  let show (b, l) =
    if is_empty (b, l) then "ε" 
    else 
      let rec show_rec n m l s =
        if l = 0 then s
        else if n = m then s
        else if n > m then show_rec (n mod m) (m/2) (l-1) (s ^ "1")
        else show_rec (n mod m) (m/2) (l-1) (s ^ "0")
      in
      show_rec b (pow2 !mlen) l ""

  let print (b, l) = show (b, l) |> print_string

  let length (b, l) = l

  let extend (b, l) n =
    if l < n then
      (b + pow2 (!mlen - n), n)
    else
      (b, l)

  let cut (b, l) =
    let rec get_length n m len = 
      if n mod m = m/2 then len
      else get_length n (2*m) (len - 1)
    in
    let rec get_value n p b =
      if n mod 2 = 1 then b + p
      else get_value (n/2) (2*p) b
    in
    let value = get_value b 1 b in
    let length = get_length value 2 !mlen in
    if value >= pow2 (!mlen + 1) then
      (** Empty **)
      (pow2 !mlen, 0)
    else
      (value, length)

  let compare (b1, l1) (b2, l2) =
    if b1 > b2 then 1
    else if b1 = b2 then 0
    else -1
end

module AdaptiveCounter = struct
  type t = ACounter of BString.t array | Top

  let d = ref 0

  let set_d x =
    if x mod 2 <> 0 then failwith "Value of d should be even" else
    d := x

  let empty = ACounter [||]

  let create list = ACounter (Array.of_list list)

  let create_top = Top

  let show = function
    | Top -> "T"
    | ACounter arr ->
      let list = Array.to_list arr in
      let rec loop = function
        | [] -> ""
        | [x] -> BString.show x
        | hd :: tl -> BString.show hd ^ "," ^ loop tl
      in
      "(" ^ loop list ^ ")"

  let print ac = show ac |> print_string

  let length = function
    | Top -> failwith "length called on AdaptiveCounter.Top"
    | ACounter arr -> Array.length arr

  let length_BStr = function
    | Top -> failwith "length_BStr called on AdaptiveCounter.Top"
    | ACounter arr -> Array.fold_left (fun acc x -> acc + BString.length x) 0 arr

  let last_index ac = (!d - 1) - 2 * (length ac - 1)

  let trim ac n =
    match ac with
    | Top -> Top
    | ACounter arr ->
      if Array.length arr = 0 then ACounter (Array.copy arr) else
      let last_idx = last_index ac in
      if last_idx >= n then ACounter (Array.copy arr)
      else 
        let to_remove = (float_of_int n -. float_of_int last_idx) /. 2. |> ceil |> int_of_float in
        let new_arr = Array.sub arr 0 (Array.length arr - to_remove) in
        ACounter new_arr

  let compare ac1 ac2 =
    match ac1, ac2 with
    | Top, Top -> 0
    | Top, ACounter arr -> 1
    | ACounter arr, Top -> -1
    | (ACounter arr1 as ac1), (ACounter arr2 as ac2) ->
    let i = ref 0 in
    let ret = ref 0 in
    while !ret = 0 && !i < length ac1 && !i < length ac2 do
      ret := BString.compare arr1.(!i) arr2.(!i);
      (* log_debug ("Returned " ^ string_of_int !ret); *)
      i := !i + 1
    done;
    if !ret = 0 && length ac1 < length ac2 then -1
    else if !ret = 0 && length ac1 > length ac2 then 1
    else !ret

  let trim_compare ac1 ac2 pi =
    match ac1, ac2 with
    | Top, Top -> 0
    | Top, ACounter arr -> 1
    | ACounter arr, Top -> -1
    | (ACounter arr1 as ac1), (ACounter arr2 as ac2) ->
      (*print_string "Comparing "; print_string (show ac1); print_string " and "; print_string (show ac2); print_newline ();*)
      let rec comp idx =
        if !d - 2 * idx - 1 < pi then 0
        else if idx >= length ac1 then (if idx >= length ac2 then 0 else -1)
        else if idx >= length ac2 then 1
        else
          let bcomp = BString.compare arr1.(idx) arr2.(idx) in
          if bcomp <> 0 then bcomp
          else comp (idx + 1)
      in
      let result = comp 0 in
      (*print_string "Result: "; print_int result; print_newline ();*)
      result

  let max ac1 ac2 =
    if compare ac1 ac2 > 0 then ac1 else ac2

  let min ac1 ac2 =
    if compare ac1 ac2 < 0 then ac1 else ac2

  let append ac bstr = 
    match ac with
    | Top -> failwith "append called on AdaptiveCounter.Top"
    | ACounter arr -> ACounter (Array.append arr [|bstr|])

  let get_last = function
    | Top -> failwith "get_last called on AdaptiveCounter.Top"
    | ACounter arr -> arr.(Array.length arr - 1)

  let remove_last = function
    | Top -> failwith "remove_last called on AdaptiveCounter.Top"
    | ACounter arr -> ACounter (Array.sub arr 0 (Array.length arr - 1))

  let trim_to_last_nonempty ac =
    match ac with
    | Top -> failwith "trim_to_last_nonempty called on AdaptiveCounter.Top"
    | ACounter arr -> 
      let i = ref (Array.length arr - 1) in
      let ret = ref false in
      while not !ret && !i >= 0 do
        if BString.is_empty arr.(!i) then i := !i - 1
        else ret := true
      done;
      ACounter (Array.sub arr 0 (!i + 1))

  let set ac n bstr =
    match ac with
    | Top -> failwith "set called on AdaptiveCounter.Top"
    | ACounter arr ->
      if Array.length arr <= n then failwith "index out of bounds"
      else arr.(n) <- bstr

  let set_last ac bstr =
    set ac (length ac - 1) bstr

  let is_max = function
    | Top -> true
    | ACounter _ -> false

  let is_empty ac = length ac = 0
end

module ProgressMeasure = struct
  type t = AdaptiveCounter.t array

  let d = ref 0

  let mu = ref 0

  let clog_mu = ref 0

  let size = Array.length

  let show pm =
    let str = ref "" in
    Array.iteri (fun node ac -> str := !str ^ "Node " ^ Paritygame.nd_show node ^ " has AC " ^ AdaptiveCounter.show ac ^ "\n") pm;
    !str

  let create pg nodes = 
    (* log_debug "Creating ProgressMeasure"; *)
    let maxPriority = Paritygame.ns_fold 
      (fun acc node -> max acc (Paritygame.pg_get_priority pg node)) 0 nodes
    in
    (* log_debug ("Highest priority found: " ^ string_of_int maxPriority); *)
    d := if maxPriority mod 2 = 0 then maxPriority else maxPriority + 1;
    (* log_debug ("Value of d: " ^ string_of_int !d); *)
    (** Set d in the AdaptiveCounter as well **)
    AdaptiveCounter.set_d !d;
    mu := Paritygame.ns_fold (fun acc node -> acc + (Paritygame.pg_get_priority pg node) mod 2) 0 nodes;
    clog_mu := clog2 !mu;
    BString.set_mu !clog_mu;
    (* log_debug ("Value of μ: " ^ string_of_int !mu); *)
    let ht = Array.make (Paritygame.ns_size nodes) AdaptiveCounter.empty in
    (* log_debug ("Size of the hashtable: " ^ (string_of_int (Hashtbl.length ht))); *)
    ht

  let get_AC pm node = pm.(node)

  let is_edge_progressive pm pg n1 n2 =
    let module AC = AdaptiveCounter in
    let ac1 = get_AC pm n1 in 
    let ac2 = get_AC pm n2 in
    let p = Paritygame.pg_get_priority pg n1 in
    if AC.is_max ac1 && AC.is_max ac2 then true
    else if p mod 2 = 0 then AC.trim_compare ac1 ac2 p >= 0
    else AC.trim_compare ac1 ac2 p > 0

  let is_node_progressive pm pg node =
    let owner = Paritygame.pg_get_owner pg node in
    let successors = Paritygame.pg_get_successors pg node in
    if owner = Paritygame.plr_Even then
      (** Exists **)
      Paritygame.ns_exists (fun succ -> is_edge_progressive pm pg node succ) successors
    else
      (** For all **)
      Paritygame.ns_forall (fun succ -> is_edge_progressive pm pg node succ) successors

  let lift_ pm pg node neighbour =
    (** Page 14-15 of the paper **)
    (* log_debug ("Calling lift_ on the edge (" ^ nd_show node ^ "," ^ nd_show neighbour ^ ")"); *)
    let module AC = AdaptiveCounter in
    let module BS = BString in
    let neighbourAC = get_AC pm neighbour in
    (* log_debug ("Neighbour's AC: " ^ AC.show neighbourAC); *)
    if AC.is_max neighbourAC then
      AC.Top
    else
    let k = AC.last_index neighbourAC in
    let p = Paritygame.pg_get_priority pg node in
    let trimmedNAC = AC.trim neighbourAC p in
    (* log_debug ("Neighbour's trimmed AC: " ^ AC.show trimmedNAC); *)
    if p mod 2 = 0 then
    begin
      let ret = AC.max trimmedNAC (get_AC pm node) in
      (* log_debug ("Even priority, returning max: " ^ AC.show ret); *)
      ret
    end
    else if k > p then (** Append 0s to the total max length **)
    begin      
      (* log_debug "k is greater than priority, appending zeros"; *)
      let toAppend = !clog_mu - AC.length_BStr neighbourAC in
      let appendedBS = BS.create_len toAppend in
      AC.append neighbourAC appendedBS
    end
    else if AC.length_BStr trimmedNAC < !clog_mu then
    begin
      (* log_debug "AdaptiveCounter not fully filled in, filling in"; *)
      let last_elt = AC.get_last trimmedNAC in
      let extension_len = !clog_mu - (AC.length_BStr trimmedNAC - BS.length last_elt) in
      let extended_last = BS.extend last_elt extension_len in
      (* log_debug ("Extended last: " ^ BS.show extended_last); *)
      AC.set_last trimmedNAC extended_last;
      (* log_debug ("lift_ returned " ^ AC.show trimmedNAC); *)
      trimmedNAC
    end
    else
    let trimmed_to_nonempty = AC.trim_to_last_nonempty trimmedNAC in
    (* log_debug ("Trimmed to non-empty: " ^ AC.show trimmed_to_nonempty); *)
    (** If the resulting AdaptiveCounter at this step is empty
        then the next biggest must be Top:
        if this is empty then clog_mu must be 0 i.e. we encountered
        an AdaptiveCounter of only ε's and since k <= p, it was
        maximal. Thus the next smallest thing is Top **)
    if AC.is_empty trimmed_to_nonempty then
    begin
      (* log_debug "Trimmed to non-empty is empty, next smallest is Top"; *)
      AC.Top
    end 
    else
    let last_in_trimmed = AC.get_last trimmed_to_nonempty in
    (* log_debug ("Last in trimmed: " ^ BS.show last_in_trimmed); *)
    if not (BS.is_max last_in_trimmed) then
      let cut_last = BS.cut last_in_trimmed in
      (* log_debug ("BString after cut: " ^ BS.show cut_last); *)
      AC.set_last trimmed_to_nonempty cut_last;
      trimmed_to_nonempty
    else if AC.length trimmed_to_nonempty > 1 then
    (** TODO: This had a bug e.g. in (e, 11) case where it wanted
              to trim more when we just want to remove last **)
    begin
      (** Remove last and set the second to last appropriately **)
      let removed_last = AC.remove_last trimmed_to_nonempty in
      let last_in_shortened = AC.get_last removed_last in
      (** TODO: This is complicated lol make it easier to read **)
      let extension_len = !clog_mu - (AC.length_BStr removed_last - BS.length last_in_shortened) in
      let extended_last = BS.extend last_in_shortened extension_len in
      AC.set_last removed_last extended_last;
      (* log_debug ("lift_ returned " ^ AC.show removed_last); *)
      removed_last
    end
    else
      AC.Top

  let lift pm pg node =
    let module AC = AdaptiveCounter in
    let module PG = Paritygame in
    (* log_debug ("Calling lift on node " ^ nd_show node); *)
    let neighbours = PG.pg_get_successors pg node |> PG.ns_nodes in
    let newAC = 
    if PG.pg_get_owner pg node = PG.plr_Even then (** Minimum **)
      List.fold_left (fun acc x -> AC.min acc (lift_ pm pg node x)) AC.Top neighbours
    else (** Maximum **)
      List.fold_left (fun acc x -> AC.max acc (lift_ pm pg node x)) AC.empty neighbours
    in
    (* log_debug ("New AC for that node is " ^ AC.show newAC); *)
    pm.(node) <- newAC

  let get_winning_set pm pg = 
    Paritygame.sol_init pg 
      (fun node ->
        let node_AC = get_AC pm node in
        (* log_debug ("Considering node " ^ nd_show node ^ " with AC " ^ AdaptiveCounter.show node_AC); *)
        match node_AC with
        | AdaptiveCounter.Top -> Paritygame.plr_Odd
        | AdaptiveCounter.ACounter _ -> Paritygame.plr_Even)

  let get_winning_strategy pm pg nodes = 
    let module PG = Paritygame in
    let str = PG.str_make (PG.ns_size nodes) in
    PG.str_iter 
      (fun n1 n2 -> 
        if PG.pg_get_owner pg n1 = PG.plr_Even && not (AdaptiveCounter.is_max (get_AC pm n1)) then
          (** Find progressive successor **)
          let successors = PG.pg_get_successors pg n1 in
          PG.str_set str n1 (PG.ns_find (fun succ -> is_edge_progressive pm pg n1 succ) successors)
      ) 
      str;
    str
end

let solve' (pg : Paritygame.paritygame) : (Paritygame.solution * Paritygame.strategy) =
  (*Paritygame.pg_iterate (fun node _ ->
    log_debug (
      "Node " ^ 
      Paritygame.nd_show node ^ 
      " with priority " ^
      (Paritygame.pg_get_priority pg node |> string_of_int) ^
      " belonging to player " ^
      (if Paritygame.pg_get_owner pg node = Paritygame.plr_Even then "Even" else "Odd") ^
      " with successors [" ^ 
      List.fold_left (fun acc x -> acc ^ "," ^ Paritygame.nd_show x) "" (Paritygame.pg_get_successors pg node |> Paritygame.ns_nodes) ^
      "]"))
    pg;*)
  let module PG = Paritygame in
  let module PM = ProgressMeasure in
  (** Initialise the progress measure **)
  let nodes = PG.collect_nodes pg (fun _ _ -> true) in
  let pm = PM.create pg nodes in
  (** Get all non-progressive **)
  let nonprog = ref (PG.ns_filter (fun node -> PG.pg_get_priority pg node mod 2 = 1) nodes) in
  (** Call lift on non-progressive until all progressive **)
  while not (PG.ns_isEmpty !nonprog) do
    (* log_debug ("### New loop: " ^ string_of_int (PG.ns_size !nonprog) ^ " non-progressive");
    log_debug (ProgressMeasure.show pm); *)
    let non_prog_node = PG.ns_some !nonprog in
    (** TODO: This may be slow **)
    nonprog := PG.ns_del non_prog_node !nonprog;
    (* log_debug ("Picked non-progressive node " ^ nd_show non_prog_node); *)
    PM.lift pm pg non_prog_node;
    (** Add predecessors that became non-progressive to the list **)
    (* log_debug "Checking if predecessors became non-progressive"; *)
    let pred = PG.pg_get_predecessors pg non_prog_node in
    PG.ns_iter (fun node ->
      if PG.pg_get_owner pg node = PG.plr_Odd then
      begin
        if not (PM.is_edge_progressive pm pg node non_prog_node)
        then nonprog := PG.ns_add node !nonprog
      end
      else
      begin
        if not (PM.is_node_progressive pm pg node)
        then nonprog := PG.ns_add node !nonprog
      end) pred;
  done;
  log_info "All nodes progressive, finishing up";
  let sol = PM.get_winning_set pm pg in
  let str = PM.get_winning_strategy pm pg nodes in
  sol, str

let invert pg =
  (** Invert the game by adding one to each priority
      and changing the ownership of nodes **)
  (* log_debug "Inverting the game"; *)
  Paritygame.pg_init
    (Paritygame.pg_size pg)
    (fun node ->
      (
        (** Increase priority by 1 **)
        1 + Paritygame.pg_get_priority pg node,
        (** Change the ownership of each node to match up the priority change **)
        Paritygame.plr_opponent (Paritygame.pg_get_owner pg node),
        (** Keep the successors and description **)
        Paritygame.ns_nodes (Paritygame.pg_get_successors pg node),
        Paritygame.pg_get_desc pg node
      )
    )

let solve game = 
  let module PG = Paritygame in
  let open Univsolve in
  universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) 
  (fun pg -> 
    (** Get node count for each player to determine which game to solve first
        hoping for a speedup **)
    let even_nodes_count = PG.collect_nodes_by_prio game (fun p -> p mod 2 = 0) |> PG.ns_size in
    let odd_nodes_count = PG.collect_nodes_by_prio game (fun p -> p mod 2 = 1) |> PG.ns_size in
    (** Get solution and strategy for the player that has more nodes in the game **)
    let (sol, str) =
      if even_nodes_count > odd_nodes_count then
        (** Even has more nodes, solve normally for even **)
        solve' pg
      else
        (** Odd has more nodes, invert and then solve **)
        let (sol_inv, str_inv) = invert pg |> solve' in
        (** Invert the winning sets to get the ones for the original game **)
        PG.sol_iter (fun node player ->
          PG.sol_set sol_inv node (PG.plr_opponent player))
        sol_inv;
        (sol_inv, str_inv)
    in
    let solved_for = if even_nodes_count > odd_nodes_count then PG.plr_Even else PG.plr_Odd in
    (** Create a subgame with all the winning nodes for the other player **)
    let (subgame, map_to_sub, map_to_game) = PG.subgame_by_node_filter pg (fun node -> sol.(node) <> solved_for) in
    if even_nodes_count > odd_nodes_count then
      (** Created a subgame for Odd, need to invert it and then solve **)
      let inverted = invert subgame in
      let (_, str_sub_inv) = solve' inverted in
      (** Combine this strategy for Odd with the strategy for Even **)
      PG.str_iter
        (fun n1 n2 ->
          if PG.pg_get_owner inverted n1 = PG.plr_Even
          then PG.str_set str (map_to_game n1) (map_to_game n2)
        )
      str_sub_inv;
    else
      (** Created a subgame for Even, no need to invert stuff **)
      let (_, str_sub) = solve' subgame in
      (** Combine this strategy for Even with the strategy for Odd **)
      PG.str_iter
        (fun n1 n2 ->
          if PG.pg_get_owner subgame n1 = PG.plr_Even
          then PG.str_set str (map_to_game n1) (map_to_game n2)
        )
      str_sub;
    ;
    (sol, str)
  ) 
  game

let register () =
  Solverregistry.register_solver
    solve
    "succsmallpm"
    "progm"
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"