val solve : Paritygame.paritygame -> Paritygame.solution * Paritygame.strategy
val register: unit -> unit

module RegisterGame : sig
  (** Module for working with register games. The register
      games are represented as normal parity games, as
      described in 
      https://www.informatik.uni-kiel.de/~kleh/register-index.pdf
      
      We are assuming so far that nodes in the original game
      have consecutive indices starting from 0 (which I believe
      is actually the case) and that the priorities start
      from 0 **)

  type t

  (** The size of the set of priorities of the original game,
      |I| in the paper **)
  val s : int ref

  (** Number of registers **)
  val k : int ref

  (** Given a paritygame , convert to a register game
      with priorities bounded by log(n) + 1 where n is
      the number of nodes in the original game **)
  val convert     : Paritygame.paritygame -> t

  (** Recover a solution for the original game from the 
      solved register game **)
  val recover_sol : t -> Paritygame.solution

  (** Recover a strategy for the original game from the 
      solved register game **)
  val recover_str : t -> Paritygame.strategy

  (** Determine the register contents from an integer
      that the contents are stored as. The register 
      contents are just an s-ary representation of the
      given number:
      
      idx = x_1 + s * x_2 + s^2 * x_3 + ... + s^(n-1) * x_n **)
  val get_register_contents : Paritygame.node -> int array

  (** A map from the index of a register game node to
      the underlying node in the original game
      
      TODO: Describe the mapping here or below **)
  val idx_to_node : Paritygame.node -> int

  (** A map from the index of a register game node to
      its description, i.e. (v, x, t), where
      v is the vertex from the original game
      x is the content of the registers
      t is the indicator for reset/move **)
  val idx_to_desc : Paritygame.node -> int * int array * int
end