open Basics;;
open Paritygame;;

let log_debug msg = message_autotagged 3 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;
let log_verb msg = message_autotagged 2 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;
let log_info msg = message_autotagged 2 (fun _ -> "SSMP") (fun _ -> msg ^ "\n") ;;

module BString = struct
  type t = BString of bool array | Empty

  let create = function
    | [] -> Empty
    | list -> BString (Array.of_list list)

  let createLen = function
    | 0 -> Empty
    | l -> BString (Array.make l false)

  let show = function
    | Empty -> "ε"
    | BString arr ->
      let list = Array.to_list arr in
      let rec loop = function
        | [] -> ""
        | hd :: tl -> (if hd then "1" else "0") ^ loop tl
      in
      loop list

  let print bstr = print_string (show bstr)

  let length = function
    | Empty -> 0
    | BString list -> Array.length list

  let append bstr n =
    if length bstr < n then
      match bstr with
      | Empty ->
        let arr = Array.make n false in
        arr.(0) <- true;
        BString arr
      | BString bstr as bstring ->
        let arr = Array.make (n - (length bstring)) false in
        arr.(0) <- true;
        BString (Array.append bstr arr)
    else
      bstr

  let cut bstr =
    (** TODO: Check if this works lol **)
    log_info ("Calling cut on BString " ^ show bstr);
    match bstr with
    | Empty -> Empty
    | BString arr as bstring ->
      let last_one = ref ((length bstring) - 1) in
      while arr.(!last_one) do
        last_one := !last_one - 1
      done;
      let new_arr = Array.sub arr 0 !last_one in
      if Array.length new_arr = 0 then Empty
      else BString (Array.sub arr 0 !last_one)

  let is_max = function
    | Empty -> false (** TODO: Check if correct **)
    | BString arr ->
      Array.for_all (fun x -> x) arr

  let compare bstr1 bstr2 =
    match bstr1, bstr2 with
    | Empty, Empty       -> 0
    | Empty, BString arr -> if arr.(0) then -1 else 1
    | BString arr, Empty -> if arr.(0) then 1 else -1
    | (BString arr1 as b1), (BString arr2 as b2) ->
      let i = ref 0 in
      let ret = ref 0 in
      while !ret = 0 && !i < length b1 && !i < length b2 do 
        if arr1.(!i) && not arr2.(!i) then ret := 1
        else if not arr1.(!i) && arr2.(!i) then ret := -1
        else
          i := !i + 1;
      done;
      if not (!ret = 0) then !ret (** Last was different **)
      else if !i = length b1 && !i = length b2 then !ret (** Reached the end in both so identical **)
      else if !i < length b1 && arr1.(!i) then 1         (** [b1] of form [b2]1..., so bigger than [b2] **)
      else if !i < length b1 then -1                     (** [b1] of form [b2]0..., so smaller than [b2] **)
      else if !i < length b2 && arr2.(!i) then -1        (** [b2] of form [b1]1..., bigger than [b1] **)
      else 1                                             (** [b2] of form [b1]0..., so smaller than [b1] **)
end

module AdaptiveCounter = struct
  type t = ACounter of BString.t array | Top

  let d = ref 0

  let set_d x =
    if x mod 2 <> 0 then failwith "Value of d should be even" else
    d := x

  let empty = ACounter [||]

  let create list = ACounter (Array.of_list list)

  let createTop = Top

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

  let lengthBS = function
    | Top -> failwith "lengthBS called on AdaptiveCounter.Top"
    | ACounter arr -> Array.fold_left (fun acc x -> acc + BString.length x) 0 arr

  let last_index ac = (!d - 1) - 2 * (length ac - 1)

  let trim ac n =
    (** TODO: Test this **)
    log_info ("Calling trim on " ^ show ac ^ " with π(n) = " ^ string_of_int n);
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
    | Top, Top -> 1 (** TODO: Check this: true because Top Top is progressive **)
    | Top, ACounter arr -> 1
    | ACounter arr, Top -> -1
    | (ACounter arr1 as ac1), (ACounter arr2 as ac2) ->
    let i = ref 0 in
    let ret = ref 0 in
    while !ret == 0 && !i < length ac1 && !i < length ac2 do
      ret := BString.compare arr1.(!i) arr2.(!i);
      i := !i + 1
    done;
    if !ret = 0 && length ac1 < length ac2 then -1
    else if !ret = 0 && length ac1 > length ac2 then 1
    else !ret

  let max ac1 ac2 =
    if compare ac1 ac2 > 0 then ac1 else ac2

  let min ac1 ac2 =
    if compare ac1 ac2 < 0 then ac1 else ac2

  let append ac bstr = 
    match ac with
    | Top -> failwith "append called on AdaptiveCounter.Top"
    | ACounter arr -> ACounter (Array.append arr [|bstr|])

  let getLast = function
    | Top -> failwith "getLast called on AdaptiveCounter.Top"
    | ACounter arr -> arr.(Array.length arr - 1)

  let trim_to_last_nonempty ac =
    log_info "Calling trim_to_last_nonempty";
    match ac with
    (** TODO: Test if this works lol **)
    | Top -> failwith "trim_to_last_nonempty called on AdaptiveCounter.Top"
    | ACounter arr -> 
      let i = ref (Array.length arr - 1) in
      let ret = ref false in
      while not !ret && !i >= 0 do
        match arr.(!i) with
        | Empty -> i := !i - 1
        | BString _ -> ret := true
      done;
      ACounter (Array.sub arr 0 (!i + 1))

  let set ac n bstr =
    match ac with
    | Top -> failwith "set called on AdaptiveCounter.Top"
    | ACounter arr ->
      if Array.length arr <= n then failwith "index out of bounds"
      else arr.(n) <- bstr

  let isMax = function
    | Top -> true
    | ACounter _ -> false
end

(** Helper function since OCaml is poor **)
let log2 x = log10 x /. log10 2.

module ProgressMeasure = struct
  type t = (Paritygame.node, AdaptiveCounter.t) Hashtbl.t

  let d = ref 0

  let mu = ref 0

  let size = Hashtbl.length

  let show pm =
    Hashtbl.fold (fun node ac acc -> acc ^ "Node " ^ Paritygame.nd_show node ^ " has AC " ^ AdaptiveCounter.show ac ^ "\n") pm ""

  let create pg nodes = 
    log_debug "Creating ProgressMeasure";
    let maxPriority = Paritygame.ns_fold 
      (fun acc node -> max acc (Paritygame.pg_get_priority pg node)) 0 nodes
    in
    log_debug ("Highest priority found: " ^ string_of_int maxPriority);
    d := if maxPriority mod 2 = 0 then maxPriority else maxPriority + 1;
    log_debug ("Value of d: " ^ string_of_int !d);
    (** Set d in the AdaptiveCounter as well **)
    AdaptiveCounter.set_d !d;
    mu := Paritygame.ns_fold (fun acc node -> acc + (Paritygame.pg_get_priority pg node) mod 2) 0 nodes;
    log_debug ("Value of μ: " ^ string_of_int !mu);
    let ht = Hashtbl.create (Paritygame.ns_size nodes) in
    Paritygame.ns_iter (fun node -> Hashtbl.add ht node AdaptiveCounter.empty) nodes;
    log_debug ("Size of the hashtable: " ^ (string_of_int (Hashtbl.length ht)));
    ht

  let lift_ pm pg node neighbour =
    (** TODO: What if neighbourAC is top **)
    (** Page 14-15 of the paper **)
    log_debug ("Calling lift_ on the edge (" ^ nd_show node ^ "," ^ nd_show neighbour ^ ")");
    let module AC = AdaptiveCounter in
    let module BS = BString in
    let neighbourAC = Hashtbl.find pm neighbour in
    log_debug ("Neighbour's AC: " ^ AC.show neighbourAC);
    if AC.isMax neighbourAC then
      AC.Top
    else
    let k = AC.last_index neighbourAC in
    let p = Paritygame.pg_get_priority pg node in
    let trimmedNAC = AC.trim neighbourAC p in
    log_debug ("Neighbour's trimmed AC: " ^ AC.show trimmedNAC);
    if p mod 2 = 0 then
    begin
      let ret = AC.max trimmedNAC (Hashtbl.find pm node) in
      log_debug ("Even priority, returning max: " ^ AC.show ret);
      (** TODO: Check this but should be fine as trimmedNAC
          is minimum required for it to be progressive **)
      ret
    end
    else 
    let clog_mu = float_of_int !mu |> log2 |> ceil |> int_of_float in
    log_debug ("Value of k is " ^ string_of_int k);
    log_debug ("Value of k is " ^ string_of_int k);
    log_debug ("Value of clog_mu is " ^ string_of_int clog_mu);
    if k > p then (** Append 0s to the total max length **)
    begin      
      log_debug "k is greater than priority, appending zeros";
      (** TODO: Check if this works lol **)
      let toAppend = clog_mu - AC.lengthBS neighbourAC in
      let appendedBS = BS.createLen toAppend in
      AC.append neighbourAC appendedBS
    end
    else if AC.lengthBS trimmedNAC < clog_mu then
    begin
      log_debug "AdaptiveCounter not fully filled in, filling in";
      let last_elt = AC.getLast trimmedNAC in
      (** TODO: Check this: wrong interpretation of append **)
      let extension_len = clog_mu - AC.lengthBS trimmedNAC in
      let extended_last = BS.append last_elt extension_len in
      AC.set trimmedNAC (AC.length trimmedNAC - 1) extended_last;
      trimmedNAC
    end
    else
    let trimmed_to_nonempty = AC.trim_to_last_nonempty trimmedNAC in
    log_debug ("Trimmed to non-empty: " ^ AC.show trimmed_to_nonempty);
    let last_in_trimmed = AC.getLast trimmed_to_nonempty in
    log_debug ("Last in trimmed: " ^ BS.show last_in_trimmed);
    if not (BS.is_max last_in_trimmed) then
      let cut_last = BS.cut last_in_trimmed in
      log_debug ("BString after cut: " ^ BS.show cut_last);
      AC.set trimmed_to_nonempty (AC.length trimmed_to_nonempty - 1) cut_last;
      trimmed_to_nonempty
    else if AC.length trimmed_to_nonempty > 1 then
      (** Remove last and set the second to last appropriately **)
      let extension_len = AC.getLast trimmed_to_nonempty |> BS.length in
      AC.set trimmed_to_nonempty (AC.length trimmed_to_nonempty - 1) (BS.create []);
      let new_trimmed = AC.trim_to_last_nonempty trimmed_to_nonempty in
      let last_in_trimmed = AC.getLast new_trimmed in
      let extended_last = BS.append last_in_trimmed extension_len in
      AC.set new_trimmed (AC.length new_trimmed - 1) extended_last;
      new_trimmed
    else
      AC.Top

  let lift pm pg node =
    let module AC = AdaptiveCounter in
    let module PG = Paritygame in
    log_info ("Calling lift on node " ^ nd_show node);
    let neighbours = PG.pg_get_successors pg node |> PG.ns_nodes in
    let newAC = 
    if PG.pg_get_owner pg node = PG.plr_Even then (** Minimum **)
      List.fold_left (fun acc x -> AC.min acc (lift_ pm pg node x)) AC.Top neighbours
    else (** Maximum **)
      List.fold_left (fun acc x -> AC.max acc (lift_ pm pg node x)) AC.empty neighbours
    in
    log_info ("New AC for that node is " ^ AC.show newAC);
    Hashtbl.replace pm node newAC

  let getAC pm node = Hashtbl.find pm node

  (** TODO: Implement lol **)
  let getWinningSet pm = Paritygame.sol_make 0

  (** TODO: Implement lol **)
  let getWinningStrategy pm = Paritygame.str_make 0

  let is_edge_progressive pm pg n1 n2 =
    if Paritygame.ns_elem n2 (Paritygame.pg_get_successors pg n1) then
      let ac1 = Hashtbl.find pm n1 in 
      let ac2 = Hashtbl.find pm n2 in
      let p = Paritygame.pg_get_priority pg n1 in
      let trimmed_ac1 = AdaptiveCounter.trim ac1 p in
      let trimmed_ac2 = AdaptiveCounter.trim ac2 p in
      if p mod 2 = 0 then AdaptiveCounter.compare trimmed_ac1 trimmed_ac2 >= 0
      else AdaptiveCounter.compare trimmed_ac1 trimmed_ac2 > 0
    else 
      failwith "Non-existing edge cannot be progressive"

  let is_node_progressive pm pg node =
    let owner = Paritygame.pg_get_owner pg node in
    let successors = Paritygame.pg_get_successors pg node in
    if owner = Paritygame.plr_Even then
      (** Exists **)
      Paritygame.ns_exists (fun succ -> is_edge_progressive pm pg node succ) successors
    else
      (** For all **)
      Paritygame.ns_forall (fun succ -> is_edge_progressive pm pg node succ) successors
end

(** TODO: Implement strategy return **)
let solve' (pg : Paritygame.paritygame) : (Paritygame.solution * Paritygame.strategy) =
  let module PG = Paritygame in
  let module PM = ProgressMeasure in
  (** Initialise the progress measure **)
  let nodes = PG.collect_nodes pg (fun _ _ -> true) in
  let pm = PM.create pg nodes in
  (** Strategy for even where if edge is non-progressive
      the node is set to nd_undef. TODO: Use **)
  (** let strategy = PG.str_create pg in **)
  (** Get all non-progressive **)
  let nonprog = ref (PG.ns_filter (fun node -> not (PM.is_node_progressive pm pg node)) nodes) in
  (** Call lift on non-progressive until all progressive **)
  while not (PG.ns_isEmpty !nonprog) do
    log_debug ("### New loop: " ^ string_of_int (PG.ns_size !nonprog) ^ " non-progressive");
    log_debug (ProgressMeasure.show pm);
    let non_prog_node = PG.ns_some !nonprog in
    (** TODO: This may be slow **)
    nonprog := PG.ns_del non_prog_node !nonprog;
    log_debug ("Picked non-progressive node " ^ nd_show non_prog_node);
    PM.lift pm pg non_prog_node;
    (** TODO: Optimise by keeping track of strategy **)
    (** Add predecessors that became non-progressive to the stack **)
    log_debug "Checking if predecessors became non-progressive";
    let pred = PG.pg_get_predecessors pg non_prog_node in
    nonprog := PG.ns_filter (fun node -> not (PM.is_node_progressive pm pg node)) pred |>
                PG.ns_fold (fun acc node -> PG.ns_add node acc) !nonprog;
  done;
  log_debug "All nodes progressive, finishing up";
  let sol = PG.sol_init pg 
    (fun node ->
      let node_AC = PM.getAC pm node in
      log_debug ("Considering node " ^ nd_show node ^ " with AC " ^ AdaptiveCounter.show node_AC);
      match node_AC with
      | AdaptiveCounter.Top -> PG.plr_Odd
      | AdaptiveCounter.ACounter _ -> PG.plr_Even);
  in
  sol, PG.str_make (PG.ns_size nodes) 

let solve game = 
  let open Univsolve in
  universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) (solve') game

let register () =
  Solverregistry.register_solver
    solve'
    "succsmallpm"
    "progm"
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"