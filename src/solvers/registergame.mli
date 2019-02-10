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

  (** Max priority in the game and consequently |I| - 1 **)
  val p : int ref

  (** Determine the register contents from an integer
      that the contents are stored as. The register 
      contents are just an s-ary representation of the
      given number:
      
      idx = x_1 + s * x_2 + s^2 * x_3 + ... + s^(n-1) * x_n **)
  val idx_to_regs : Paritygame.node -> int array

  (** Given register contents, determine the unique index
      of those contents. This is an inverse operation to
      [idx_to_regs] **)
  val regs_to_idx : int array -> Paritygame.node

  (** Given register contents and the index of the register
      being reset, determine the index of the resulting
      register contents **)
  val regs_to_idx_reset : int array -> int -> Paritygame.node

  (** Given a description of a register game node in the form
      [underlying node], [registers], [t], retrieve the index 
      of it **)
  val desc_to_idx : Paritygame.node -> int array -> int -> int

  (** Given a node from the underlying game, the register
      contents and the register to reset, give the index of
      the resulting node in the register game. The value of
      t is not needed as the resulting node will have t = 1
      always **)
  val desc_to_idx_reset : Paritygame.node -> int array -> int -> int

  (** A map from the index of a register game node to
      the underlying node in the original game
      
      TODO: Describe the mapping here or below **)
  val idx_to_node : Paritygame.node -> int

  (** Recover the value of t (0 or 1) from the index **)
  val idx_to_t : Paritygame.node -> int

  (** A map from the index of a register game node to
      its description, i.e. (v, x, t), where
      v is the vertex from the original game
      x is the content of the registers
      t is the indicator for reset/move **)
  val idx_to_desc : Paritygame.node -> int * int array * int

  (** Given registers and a priority of a new node to
      visit, update each register to hold the max value
      between it's original content and the priority **)
  val update_registers : int array -> int -> int array

  (** Given an index of a current node and its neighbour
      in the original game, determine the neighbour in the
      register game
      i.e. if current node is (v, x, 1) and v' is a neighbour
      of v in the original game, then we return the index
      of (v', x', 0), where x' are the updated registers after
      moving to a new vertex **)
  val get_rg_neighbour : int -> Paritygame.node -> Paritygame.paritygame -> int

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
end