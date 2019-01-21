(*open Paritygame

let solve (game : Paritygame.paritygame) = (sol_make 0, str_make 0)

let register () =
  Solverregistry.register_solver
    solve
    "succsmallpm"
    "sspm"
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"*)

let solve (game : Paritygame.paritygame) = (Paritygame.sol_make 0, Paritygame.str_make 0)
let register () = ()