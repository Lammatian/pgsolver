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

module RegisterGame = struct
  type t = Paritygame.paritygame

  (** TODO: Check if priorities start from 0 **)
  let s = ref 0

  let k = ref 0

  let p = ref 0

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
    conv [] i

  let regs_to_idx regs =
    let rd = !p + 1 in
    let result = ref 0 in
    let mult = ref 1 in
    for i = 0 to Array.length regs - 1 do
      result := regs.(i) * !mult;
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
        result := regs.(i) * !mult;
        mult := !mult * rd;
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

  let idx_to_t i = if i mod (2 * !s) >= !s then 1 else 0

  let idx_to_desc i = 
    let v = idx_to_node i in
    let registers = idx_to_regs (i mod !s) in
    let t = idx_to_t i in
    (v, registers, t)

  let update_registers regs p =
    Array.map (fun r -> max r p) regs

  let get_rg_neighbour i neighbour_in_g pg =
    let registers = idx_to_regs i in 
    let priority_in_g = Paritygame.pg_get_priority pg neighbour_in_g in
    let updated_registers = update_registers registers priority_in_g in
    desc_to_idx neighbour_in_g updated_registers 0

  let convert pg =
    let module PG = Paritygame in
    let nodes = PG.collect_nodes pg (fun _ _ -> true) in
    let n = PG.ns_size nodes in
    let n_float = float_of_int n in
    k := (log2 n_float) +. 1. |> ceil |> int_of_float;
    p := PG.pg_max_prio pg;
    s := pow !p !k;
    (** TODO: Idea - mark with 'x' in description if a node has been
        visited, to then easily remove nodes with in-degree 0 **)
    (** TODO: Making separate HUGE arrays and then combining them
        doesn't seem like the best idea, try to make one array **)
    let rg_nodes = Array.make (n * !s) (0, PG.plr_undef, [], None) in
    (** Add all non-reset nodes with appripriate priority and owner **)
    PG.ns_iter (fun node ->
      for j = 0 to 2 * !s - 1 do
        (** The owner is Even if in a non-move node (t = 0),
            otherwise the owner is the owner in the original game **)
        let owner = if j < !s then PG.plr_Even else PG.pg_get_owner pg node in
        (** Priority depends on the owner **)
        let priority = if owner = PG.plr_Even then 0 else 1 in
        rg_nodes.(j) <- (priority, owner, [], None)
      done;)
      nodes;
    (** Reset node creation with appropriate successors (one for each) **)
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
        reset_nodes.(i * !k + r) <- (priority, owner, [successor], None)
      done)
      rg_nodes;
    (** Adding successors to all non-reset nodes **)
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
    (** Initialise the game **)
    PG.pg_init (Array.length rg_nodes + Array.length reset_nodes)
      (fun node ->
        if node < Array.length rg_nodes then
          rg_nodes.(node)
        else
          reset_nodes.(node - Array.length rg_nodes)
      )

  let recover_sol rg = Paritygame.sol_make 0

  let recover_str rg = Paritygame.str_make 0
end

let solve game = 
  (Paritygame.sol_make 0, Paritygame.str_make 0)

let register () =
  Solverregistry.register_solver
    solve
    "registergame"
    "rg" (** TODO: Make sure doesn't clash with anything **)
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"