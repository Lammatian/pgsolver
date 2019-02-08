module RegisterGame = struct
  type t = Paritygame.paritygame

  (** TODO: Check if priorities start from 0 **)
  let s = ref 0

  let convert pg = pg

  let recover_sol rg = Paritygame.sol_make 0

  let recover_str rg = Paritygame.str_make 0

  let get_register_contents i =
    let rec conv l x =
      if x = 0 then
        (** Finished, add trailing zeros to the result
            to make it of length s **)
        let len = !s - List.length l in
        let trailing_zeros = Array.make len 0 in
        Array.append (Array.of_list l) trailing_zeros
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