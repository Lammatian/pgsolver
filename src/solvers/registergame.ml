open Basics
open Arg
open Tcsargs

let log_debug msg = message_autotagged 3 (fun _ -> "RGAME") (fun _ -> msg ^ "\n")
let log_verb  msg = message_autotagged 2 (fun _ -> "RGAME") (fun _ -> msg ^ "\n") 
let log_info  msg = message_autotagged 2 (fun _ -> "RGAME") (fun _ -> msg ^ "\n") 

(** Helper functions because OCaml is poor **)
let log2 x = log10 x /. log10 2.

(** Taken from
    https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml
  **)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
let clog2 x =
  if x = 0 then 0 else
  let rec find y p = if y >= x then p else find (y lsl 1) (p + 1) in
  find 1 0

module Converters = struct
  let s = ref 0
  let k = ref 0
  let p = ref 0

  let regs_to_idx regs =
    let rd = !p + 1 in
    let result = ref 0 in
    let mult = ref 1 in
    for i = 0 to Array.length regs - 1 do
      result := !result + regs.(i) * !mult;
      mult := !mult * rd;
    done;
    !result

  let regs_to_idx_reset regs r =
    (** Size of the domain for each register **)
    let rd = !p + 1 in
    let result = ref 0 in
    (** Start with rd as multiplier because when resetting, we
        are starting from looking at the second register, first
        is 0 **)
    let mult = ref rd in
    for i = 0 to Array.length regs - 1 do
      if i <> r - 1 then
      begin
        result := !result + regs.(i) * !mult;
        mult := !mult * rd;
      end
    done;
    !result

  let desc_to_idx node regs t =
    let reg_idx = regs_to_idx regs in
    let node_idx = node * 2 * !s in
    let t_idx = if t = 1 then !s else 0 in
    node_idx + reg_idx + t_idx

  let desc_to_idx_reset node regs r =
    let reg_idx = regs_to_idx_reset regs r in
    let node_idx = node * 2 * !s in
    let t_idx = !s in
    node_idx + reg_idx + t_idx

  let idx_to_node i = i / (2 * !s)

  let idx_to_regs i =
    (** Size of the domain for each register **)
    let rd = !p + 1 in
    let rec conv l x =
      if x = 0 then
        (** Finished, add trailing zeros to the result
            to make it of length s **)
        let len = !k - List.length l in
        let trailing_zeros = Array.make len 0 in
        (** TODO: Is this reversing really necessary or
            could we avoid that? **)
        let rev_list = List.rev l in
        Array.append (Array.of_list rev_list) trailing_zeros
      else
        (** Add x % s to the beginning of the list and
            divide x by s **)
        conv ((x mod rd) :: l) (x / rd) 
    in
    conv [] (i mod !s)

  let idx_to_t i = if i mod (2 * !s) >= !s then 1 else 0

  let idx_to_desc i = 
    let v = idx_to_node i in
    let registers = idx_to_regs i in
    let t = idx_to_t i in
    (v, registers, t)

  let string_of_desc v regs t =
    let str_v = string_of_int v in
    let str_regs = "[" ^ String.concat "," (Array.map string_of_int regs |> Array.to_list) ^ "]" in
    let str_t = string_of_int t in
    "(" ^ String.concat ", " [str_v; str_regs; str_t] ^ ")"
end

module RegisterGame = struct
  open Converters
  type t = Paritygame.paritygame

  (** TODO: Check if priorities start from 0 **)
  let s = ref 0
  let k = ref 0
  let p = ref 0

  let update_registers regs p =
    Array.map (fun r -> max r p) regs

  let get_rg_neighbour i neighbour_in_g pg =
    let registers = idx_to_regs i in 
    let priority_in_g = Paritygame.pg_get_priority pg neighbour_in_g in
    let updated_registers = update_registers registers priority_in_g in
    desc_to_idx neighbour_in_g updated_registers 0

  let remove_unnecessary_nodes pg rg_nodes reset_nodes =
    let visited = Array.make (Array.length rg_nodes + Array.length reset_nodes) false in
    let visited_count = ref 0 in
    (** Find max priority vertex v and the according register game 
        vertex (v, [pi(v), ..., pi(v)], 0) which will definitely be
        visited infinitely often **)
    let max_prio_node = Paritygame.pg_max_prio_node pg in
    let max_prio_idx = desc_to_idx max_prio_node (Array.make !k !p) 0 in
    visited.(max_prio_idx) <- true;
    visited_count := !visited_count + 1;
    let max_prio_rgnode = rg_nodes.(max_prio_idx) in
    log_debug "Traversing the graph";
    (** Graph traversal **)
    let stack = Stack.create () in
    Stack.push max_prio_rgnode stack;
    while not (Stack.is_empty stack) do
      let _, _, succ, _ = Stack.pop stack in
      let rec update_stack = function
        | [] -> ()
        | hd :: tl ->
          (** Mark visited **)
          if not (visited.(hd)) then
          begin
            visited.(hd) <- true;
            visited_count := !visited_count + 1;
            let next =
              if hd < Array.length rg_nodes then rg_nodes.(hd)
              else reset_nodes.(hd - Array.length rg_nodes)
            in
            Stack.push next stack
          end;
          update_stack tl
      in
      update_stack succ
    done;
    let cleared_up = Array.make !visited_count (0, Paritygame.plr_undef, [], None) in
    let mapping_old_to_new = Hashtbl.create !visited_count in
    let mapping_new_to_old = Array.make !visited_count 0 in
    visited_count := 0;
    (** Create cleared up array of nodes and a new mapping **)
    log_debug "Creating a cleared up array of nodes and a map for successors";
    for i = 0 to (Array.length rg_nodes + Array.length reset_nodes - 1) do
      if visited.(i) then
      begin
        cleared_up.(!visited_count) <-
          if i < Array.length rg_nodes then rg_nodes.(i)
          else reset_nodes.(i - Array.length rg_nodes);
        (** Remember that old idx i is now index !visited_count **)
        Hashtbl.add mapping_old_to_new i !visited_count;
        (** Remember that new index !visited_count is the old i **)
        mapping_new_to_old.(!visited_count) <- i;
        visited_count := !visited_count + 1;
      end
    done;
    (** Update all successors using the mapping **)
    log_debug "Updating successors";
    for i = 0 to (Array.length cleared_up - 1) do
      let v, o, succ, desc = cleared_up.(i) in
      let rec update_successors = function
        | [] -> []
        | hd :: tl -> Hashtbl.find mapping_old_to_new hd :: update_successors tl
      in
      cleared_up.(i) <- (v, o, update_successors succ, desc);
    done;
    (cleared_up, mapping_new_to_old)

  let create pg =
    let module PG = Paritygame in
    let nodes = PG.collect_nodes pg (fun _ _ -> true) in
    let n = PG.ns_size nodes in
    k := clog2 n + 1;
    Converters.k := !k;
    (** TODO: Check if this actually finds the max priority **)
    p := PG.pg_max_prio pg;
    Converters.p := !p;
    log_debug ("Max priority: " ^ string_of_int !p);
    s := pow (!p + 1) !k;
    Converters.s := !s;
    log_debug ("k: " ^ string_of_int !k);
    log_debug ("p: " ^ string_of_int !p);
    log_debug ("s: " ^ string_of_int !s);
    (** TODO: Idea - mark with 'x' in description if a node has been
        visited, to then easily remove nodes with in-degree 0 **)
    (** TODO: Making separate HUGE arrays and then combining them
        doesn't seem like the best idea, try to make one array **)
    log_debug "Creating non-reset nodes";
    let rg_nodes = Array.make (n * 2 * !s) (0, PG.plr_undef, [], None) in
    (** Add all non-reset nodes with appripriate priority and owner.
        If verbosity at least 2, add description as well **)
    PG.ns_iter (fun node ->
      for j = 0 to 2 * !s - 1 do
        (** The owner is Even if in a non-move node (t = 0),
            otherwise the owner is the owner in the original game **)
        let owner = if j < !s then PG.plr_Even else PG.pg_get_owner pg node in
        (** Priority depends on the owner **)
        (* let priority = if owner = PG.plr_Even then 0 else 1 in *)
        let priority = 1 in
        (** Add description if verbosity >= 2 **)
        let desc = if !Basics.verbosity >= 2 then Some (string_of_desc node (idx_to_regs j) (idx_to_t j)) else None in
        (*log_debug ("Desc: " ^ match desc with Some x -> x | None -> "");*)
        rg_nodes.(node * 2 * !s + j) <- (priority, owner, [], desc)
      done;)
      nodes;
    (** Reset node creation with appropriate successors (one for each) **)
    log_debug "Creating reset nodes";
    let reset_nodes = Array.make (Array.length rg_nodes * !k) (0, PG.plr_undef, [], None) in
    Array.iteri (fun i _ ->
      for r = 1 to !k do
        (** Even owns all reset nodes **)
        let owner = PG.plr_Even in
        (** Determine the register contents of the node i **)
        let registers = idx_to_regs i in
        let priority = if registers.(r - 1) mod 2 = 0 then 2*r else 2*r + 1 in
        (** Get the successor after reset **)
        let underlying_node = idx_to_node i in
        let successor = desc_to_idx_reset underlying_node registers r in
        let desc = if !Basics.verbosity >= 2 then Some ("r" ^ string_of_int r ^ string_of_desc underlying_node registers 0) else None in
        (* log_debug ("Desc: " ^ match desc with Some x -> x | None -> ""); *)
        reset_nodes.(i * !k + r - 1) <- (priority, owner, [successor], desc)
      done)
      rg_nodes;
    (** Adding successors to all non-reset nodes **)
    log_debug "Adding successors to non-reset nodes";
    let rg_nodes = Array.mapi (fun i (prio, owner, succ, desc) ->
      let t = idx_to_t i in 
      let successors = succ in
      let node_in_g = idx_to_node i in
      if t = 0 then (** Add skip edge and reset as neighbour **)
        (** Skip edge **)
        let skip = i + !s in
        (** Each possible register reset is a neighbour **)
        let rec get_reset_edges successors r =
          if r > !k then successors
          (** We need Array.length rg_nodes because the index of
              a reset node is shifted this much (they are considered
              to have bigger indices than the non-reset nodes) **)
          else get_reset_edges ((Array.length rg_nodes + i * !k + (r - 1)) :: successors) (r + 1)
        in
        let successors = skip :: get_reset_edges successors 1 in
        (prio, owner, successors, desc)
      else
        (** Get all successors of the current node in G **)
        let successors_in_g = PG.pg_get_successors pg node_in_g in
        (** Determine all successors in RG **)
        let successors = PG.ns_fold (fun acc node ->
          (get_rg_neighbour i node pg) :: acc)
          [] successors_in_g
        in
        (prio, owner, successors, desc)
      )
      rg_nodes 
    in
    log_debug "Removing unnecessary nodes from the graph";
    let cleared_up_nodes, mapping_new_to_old = remove_unnecessary_nodes pg rg_nodes reset_nodes in
    (** Initialise the game **)
    PG.pg_init (Array.length cleared_up_nodes)
      (fun node -> cleared_up_nodes.(node)), mapping_new_to_old

  let recover_sol rg_sol mapping n =
    log_debug "Recovering solution";
    let sol = Array.make n Paritygame.plr_undef in
    Paritygame.sol_iter (fun node plr ->
        (*log_debug ("Node: " ^ string_of_int node);*)
        let original_rg_node = mapping.(node) in
        (* log_debug ("Original rg node: " ^ string_of_int original_rg_node); *)
        if original_rg_node < 2 * !s * n then (
          let original_g_node = idx_to_node original_rg_node in
          (* log_debug ("Original g node: " ^ string_of_int original_g_node); *)
          if sol.(original_g_node) <> plr && sol.(original_g_node) <> Paritygame.plr_undef 
          then failwith "Inconsistency in the solution"
          else sol.(original_g_node) <- plr
        )
      ) rg_sol;
    sol

  let recover_str pg rg_str mapping n =
    log_debug "Recovering strategy";
    let str = Array.make n Paritygame.nd_undef in
    Paritygame.str_iter (fun node strat ->
        (* log_debug ("Node: " ^ string_of_int node);
        log_debug ("Strat: " ^ string_of_int strat); *)
        if strat <> Paritygame.nd_undef then (
          let original_rg_node = mapping.(node) in
          let original_rg_strat = mapping.(strat) in
          (* log_debug ("Original rg node: " ^ string_of_int original_rg_node);
          log_debug ("Original rg strat: " ^ string_of_int original_rg_strat); *)
          if original_rg_node < 2 * !s * n then (
            let t = idx_to_t original_rg_node in
            if t = 1 then (
              let original_g_node = idx_to_node original_rg_node in
              let original_g_strat = idx_to_node original_rg_strat in
              let v, r, t = idx_to_desc original_rg_node in
              let vs, rs, ts = idx_to_desc original_rg_strat in
              log_debug ("Original g node: " ^ string_of_int original_g_node ^ "|" ^ string_of_desc v r t);
              log_debug ("Original g strat: " ^ string_of_int original_g_strat ^ "|" ^ string_of_desc vs rs ts);
              if str.(original_g_node) <> original_g_strat && str.(original_g_node) <> Paritygame.nd_undef 
              then (
                log_debug "Inconsistency in the strategy";
                if Paritygame.pg_get_owner pg original_g_node = Paritygame.plr_Even then str.(original_g_node) <- original_g_strat
              )
              else str.(original_g_node) <- original_g_strat
              (* if str.(original_g_node) = Paritygame.nd_undef then str.(original_g_node) <- original_g_strat *)
            )
          )
        )
      ) rg_str;
    str
end

let underlying_solver : Paritygame.global_solver ref = ref Recursive.solve

let solve game = 
  let n = Paritygame.collect_nodes game (fun _ _ -> true) |> Paritygame.ns_size in
  let rg, mapping = RegisterGame.create game in
  (* if !Basics.verbosity >= 2 then Paritygame.print_game rg; *)
  let rg_sol, rg_str = !underlying_solver rg in
  (* if !Basics.verbosity >= 2 then Paritygame.print_solution_strategy_parsable rg_sol rg_str; *)
  (RegisterGame.recover_sol rg_sol mapping n, RegisterGame.recover_str game rg_str mapping n)

module CommandLine = struct
  let speclist =  [(["--solver"; "-s"], String (fun i -> 
                        let (global_fact, _, _) = Solverregistry.find_solver i in
                        underlying_solver := global_fact [|""|]),
                      "<solver>\n     set underlying solver (default is recursive)");
                      ]

  let parse s = 
  	SimpleArgs.parsearr s speclist (fun _ -> ()) "Register Game\n" SimpleArgs.argprint_help SimpleArgs.argprint_bad
end ;;


open CommandLine ;;

let register () =
  Solverregistry.register_solver_factory
    (fun s -> parse s; solve)
    "registergame"
    "rg" (** TODO: Make sure doesn't clash with anything **)
    "Converter to an equivalent game with O(log n) bound on priorities but n^(log n) nodes"