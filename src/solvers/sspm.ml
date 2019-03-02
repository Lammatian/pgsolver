open Basics;;
open Paritygame;;
open Tcsqueue;;

let log_debug = message 3
let log_verb  = message 2
let log_info  = message 2

let log_debug_tagged = message_autotagged 3 (fun _ -> "SSPM")
let log_verb_tagged  = message_autotagged 2 (fun _ -> "SSPM")
let log_info_tagged  = message_autotagged 2 (fun _ -> "SSPM")

let clog2 x =
  if x = 0 then 0 else
  let rec find y p = if y >= x then p else find (y lsl 1) (p + 1) in
  find 1 0

module BString = struct
  type t = BString of bool list | Empty

  let create = function
    | [] -> Empty
    | list -> BString list

  let create_len = function
    | 0 -> Empty
    | l -> BString (List.init l (fun _ -> false))

  let show = function
    | Empty -> "ε"
    | BString list ->
      let rec loop = function
        | [] -> ""
        | hd :: tl -> (if hd then "1" else "0") ^ loop tl
      in
      loop list

  let print bstr = print_string (show bstr)

  let length = function
    | Empty -> 0
    | BString list -> List.length list

  let extend bstr n =
    if length bstr < n then
      match bstr with
      | Empty ->
        let 
          list = List.init n (fun i -> i = 0)
        in
        BString list
      | BString list ->
        let to_add = n - length bstr in
        let zeros = List.init to_add (fun i -> i = 0) in
        BString (List.append list zeros)
    else
      bstr
  
  let cut bstr =
    match bstr with
    | Empty -> Empty
    | BString list ->
      (** Cut zeros from start in reversed **)
      let rec cut_zero = function
        | [] -> []
        | hd :: tl ->
          if hd then cut_zero tl else tl
      in
      let list_cut = List.rev list |> cut_zero |> List.rev in
      if List.length list_cut = 0 then Empty
      else BString list_cut

  let is_max = function
    | Empty -> false
    | BString list ->
      List.for_all (fun x -> x) list

  let is_empty = function
    | Empty -> true
    | BString _ -> false

  let compare bstr1 bstr2 =
    match bstr1, bstr2 with
    | Empty, Empty           -> 0
    | BString list, Empty    -> if List.hd list then 1 else -1
    | Empty, BString list    -> if List.hd list then -1 else 1
    | BString l1, BString l2 ->
      let rec compare_rec l1 l2 =
        match l1, l2 with
        | [], []             -> 0
        | hd :: tl, []       -> if hd then  1 else -1
        | [], hd :: tl       -> if hd then  -1 else 1
        | h1 :: t1, h2 :: t2 ->
          if h1 && not h2 then 1
          else if not h1 && h2 then -1
          else compare_rec t1 t2
      in
      compare_rec l1 l2
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
      let rec comp idx =
        if !d - 2 * idx - 1 < pi then 0
        else if idx >= length ac1 then (if idx >= length ac2 then 0 else -1)
        else if idx >= length ac2 then 1
        else
          let bcomp = BString.compare arr1.(idx) arr2.(idx) in
          if bcomp <> 0 then bcomp
          else comp (idx + 1)
      in
      comp 0

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

  let eta = ref 0

  let max_len = ref 0

  let size = Array.length

  let show pm =
    let str = ref "" in
    Array.iteri (fun node ac -> str := !str ^ "Node " ^ Paritygame.nd_show node ^ " has AC " ^ AdaptiveCounter.show ac ^ "\n") pm;
    !str

  let create pg nodes = 
    log_debug (fun _ -> "Creating ProgressMeasure" ^ "\n");
    let maxPriority = Paritygame.ns_fold 
      (fun acc node -> max acc (Paritygame.pg_get_priority pg node)) 0 nodes
    in
    log_debug (fun _ -> "Highest priority found: " ^ string_of_int maxPriority ^ "\n");
    d := if maxPriority mod 2 = 0 then maxPriority else maxPriority + 1;
    log_debug (fun _ -> "Value of d: " ^ string_of_int !d ^ "\n");
    (** Set d in the AdaptiveCounter as well **)
    AdaptiveCounter.set_d !d;
    eta := Paritygame.ns_fold (fun acc node -> acc + (Paritygame.pg_get_priority pg node) mod 2) 0 nodes;
    max_len := clog2 !eta;
    log_debug (fun _ -> "Value of eta: " ^ string_of_int !eta ^ "\n");
    let ht = Array.make (Paritygame.ns_size nodes) AdaptiveCounter.empty in
    ht

  let get_AC pm node = pm.(node)

  let set_AC pm node ac = pm.(node) <- ac

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
      (** Even -> exists a progressive edge **)
      Paritygame.ns_exists (fun succ -> is_edge_progressive pm pg node succ) successors
    else
      (** Odd -> all edges progressive **)
      Paritygame.ns_forall (fun succ -> is_edge_progressive pm pg node succ) successors

  let lift_ pm pg node neighbour =
    (** Page 14-15 of the paper **)
    let module AC = AdaptiveCounter in
    let module BS = BString in
    let neighbourAC = get_AC pm neighbour in
    if AC.is_max neighbourAC then
      AC.Top
    else
    let k = AC.last_index neighbourAC in
    let p = Paritygame.pg_get_priority pg node in
    let trimmedNAC = AC.trim neighbourAC p in
    if p mod 2 = 0 then
      AC.max trimmedNAC (get_AC pm node)
    else if k > p then (
      (** Append 0s to the total max length **)
      let toAppend = !max_len - AC.length_BStr neighbourAC in
      let appendedBS = BS.create_len toAppend in
      AC.append neighbourAC appendedBS
    )
    else if AC.length_BStr trimmedNAC < !max_len then (
      let last_elt = AC.get_last trimmedNAC in
      let extension_len = !max_len - (AC.length_BStr trimmedNAC - BS.length last_elt) in
      let extended_last = BS.extend last_elt extension_len in
      AC.set_last trimmedNAC extended_last;
      trimmedNAC
    )
    else
    let trimmed_to_nonempty = AC.trim_to_last_nonempty trimmedNAC in
    (** If the resulting AdaptiveCounter at this step is empty
        then the next biggest must be Top:
        if this is empty then max_len must be 0 i.e. we encountered
        an AdaptiveCounter of only ε's and since k <= p, it was
        maximal. Thus the next smallest thing is Top **)
    if AC.is_empty trimmed_to_nonempty then (
      AC.Top
    )
    else
    let last_in_trimmed = AC.get_last trimmed_to_nonempty in
    if not (BS.is_max last_in_trimmed) then (
      let cut_last = BS.cut last_in_trimmed in
      AC.set_last trimmed_to_nonempty cut_last;
      trimmed_to_nonempty
    )
    else if AC.length trimmed_to_nonempty > 1 then (
      (** Remove last and set the second to last appropriately **)
      let removed_last = AC.remove_last trimmed_to_nonempty in
      let last_in_shortened = AC.get_last removed_last in
      let extension_len = !max_len - (AC.length_BStr removed_last - BS.length last_in_shortened) in
      let extended_last = BS.extend last_in_shortened extension_len in
      AC.set_last removed_last extended_last;
      removed_last
    )
    else
      AC.Top

  let lift pm pg node =
    let module AC = AdaptiveCounter in
    let module PG = Paritygame in
    log_debug (fun _ -> "Calling lift on node " ^ nd_show node ^ "\n");
    let neighbours = PG.pg_get_successors pg node |> PG.ns_nodes in
    let newAC = 
    if PG.pg_get_owner pg node = PG.plr_Even then (** Minimum **)
      List.fold_left (fun acc x -> AC.min acc (lift_ pm pg node x)) AC.Top neighbours
    else (** Maximum **)
      List.fold_left (fun acc x -> AC.max acc (lift_ pm pg node x)) AC.empty neighbours
    in
    log_debug (fun _ -> "New AC for that node is " ^ AC.show newAC ^ "\n");
    set_AC pm node newAC

  let get_winning_set pm pg = 
    log_debug (fun _ -> "Getting the winning sets" ^ "\n");
    Paritygame.sol_init pg 
      (fun node ->
        let node_AC = get_AC pm node in
        log_debug (fun _ -> "Considering node " ^ nd_show node ^ " with AC " ^ AdaptiveCounter.show node_AC ^ "\n");
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
  let module PG = Paritygame in
  let module PM = ProgressMeasure in
  (** Initialise the progress measure **)
  let nodes = PG.collect_nodes pg (fun _ _ -> true) in
  let pm = PM.create pg nodes in
  (** Get all non-progressive **)
  let nonprog = ref (PG.ns_filter (fun node -> PG.pg_get_priority pg node mod 2 = 1) nodes) in
  (** Call lift on non-progressive until all progressive **)
  while not (PG.ns_isEmpty !nonprog) do
    log_debug (fun _ -> "### New loop: " ^ string_of_int (PG.ns_size !nonprog) ^ " non-progressive ###" ^ "\n");
    log_debug (fun _ -> ProgressMeasure.show pm);
    let non_prog_node = PG.ns_some !nonprog in
    nonprog := PG.ns_del non_prog_node !nonprog;
    log_debug (fun _ -> "Picked non-progressive node " ^ nd_show non_prog_node ^ "\n");
    PM.lift pm pg non_prog_node;
    (** Add predecessors that became non-progressive to the list **)
    log_debug (fun _ -> "Checking if predecessors became non-progressive" ^ "\n");
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
  log_debug (fun _ -> "All nodes progressive, finishing up" ^ "\n");
  let sol = PM.get_winning_set pm pg in
  let str = PM.get_winning_strategy pm pg nodes in
  sol, str

let invert pg =
  (** Invert the game by adding one to each priority
      and changing the ownership of nodes **)
  log_debug (fun _ -> "Inverting the game" ^ "\n");
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
      if even_nodes_count > odd_nodes_count then (
        (** Even has more nodes, solve normally for even **)
        log_info_tagged (fun _ -> "Solving the game for Even" ^ "\n");
        solve' pg
      )
      else (
        (** Odd has more nodes, invert and then solve **)
        log_info_tagged (fun _ -> "Inverting and solving the game for Odd" ^ "\n");
        let (sol_inv, str_inv) = invert pg |> solve' in
        (** Invert the winning sets to get the ones for the original game **)
        PG.sol_iter (fun node player ->
          PG.sol_set sol_inv node (PG.plr_opponent player))
        sol_inv;
        (sol_inv, str_inv)
      )
    in
    log_info_tagged (fun _ -> "Solved the game for the first player" ^ "\n");
    let solved_for = if even_nodes_count > odd_nodes_count then PG.plr_Even else PG.plr_Odd in
    (** Create a subgame with all the winning nodes for the other player **)
    let (subgame, map_to_sub, map_to_game) = PG.subgame_by_node_filter pg (fun node -> sol.(node) <> solved_for) in
    if even_nodes_count > odd_nodes_count then (
      log_info_tagged (fun _ -> "Solving the subgame for Odd" ^ "\n");
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
    )
    else (
      log_info_tagged (fun _ -> "Solving the subgame for Even" ^ "\n");
      (** Created a subgame for Even, no need to invert stuff **)
      let (_, str_sub) = solve' subgame in
      (** Combine this strategy for Even with the strategy for Odd **)
      PG.str_iter
        (fun n1 n2 ->
          if PG.pg_get_owner subgame n1 = PG.plr_Even
          then PG.str_set str (map_to_game n1) (map_to_game n2)
        )
      str_sub;
    );
    log_info_tagged (fun _ -> "Solved the subgame, finishing up" ^ "\n");
    (sol, str)
  ) 
  game

let register () =
  Solverregistry.register_solver
    solve
    "succinctpm"
    "spm"
    "Quasi-polynomial time and quasi-linear space algorithm by Jurdzinski and Lazic (2017)"