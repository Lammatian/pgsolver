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

  let convert pg =
    let module PG = Paritygame in
    let nodes = PG.collect_nodes pg (fun _ _ -> true) in
    let n = PG.ns_size nodes in
    let n_float = float_of_int n in
    k := (log2 n_float) +. 1. |> ceil |> int_of_float;
    let max_priority = PG.pg_max_prio pg in
    s := pow max_priority !k;
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
    pg

  let recover_sol rg = Paritygame.sol_make 0

  let recover_str rg = Paritygame.str_make 0

  let get_register_contents i =
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
        conv ((x mod !s) :: l) (x / !s) 
    in
    conv [] i


  let idx_to_node i = i / (2 * !s)

  let idx_to_desc i = 
    let v = idx_to_node i in
    let registers = get_register_contents (i mod !s) in
    let t = if i mod (2 * !s) >= !s then 1 else 0 in
    (v, registers, t)
end

let solve game = 
  (Paritygame.sol_make 0, Paritygame.str_make 0)

let register () =
  Solverregistry.register_solver
    solve
    "registergame"
    "rg" (** TODO: Make sure doesn't clash with anything **)
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"