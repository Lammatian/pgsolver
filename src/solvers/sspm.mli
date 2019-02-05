(** TODO: Consistency in underscore_naming you [REDACTED] **)

val solve : Paritygame.paritygame -> Paritygame.solution * Paritygame.strategy
val register: unit -> unit

module BString : sig
  type t
  (** Create BString from a list of booleans **)
  val create     : bool list -> t

  (** Create BString of all 0s of length of the first argument **)
  val create_len : int -> t

  (** Returns the string representation of the BString **)
  val show       : t -> string

  (** Prints the BString to the standard output **)
  val print      : t -> unit

  (** Get the length of the bstring **)
  val length     : t -> int

  (** Extends the BString with 10...0 to a total length of the second argument **)
  val extend     : t -> int -> t

  (** Cut the last 01...1 from the BString **)
  val cut        : t -> t

  (** Check if BString is maximal of its length (i.e. only 1s) **)
  val is_max     : t -> bool

  (** Check if BString is empty **)
  val is_empty   : t -> bool

  (** Compare two BStrings with each other, returning
      -1 if first is smaller, 0 if they're equal and
      1 if first is bigger than the second one **)
  val compare    : t -> t -> int
end

module AdaptiveCounter : sig
  type t
  (** Smallest even number not smaller than any priority
      in our parity game **)
  val d           : int ref

  (** Set the value of d **)
  val set_d       : int -> unit

  (** Empty AdaptiveCounter (smallest possible) **)
  val empty       : t

  (** Create AdaptiveCounter from list of BStrings **)
  val create      : BString.t list -> t

  (** Create Top AdaptiveCounter **)
  val create_top  : t

  (** Get length of the AdaptiveCounter defined as
      the number of BStrings inside it **)
  val length      : t -> int

  (** Get the total length of all BStrings inside the
      AdaptiveCounter **)
  val length_BStr : t -> int

  (** Get last index of the given AdaptiveCounter counted
      as in the paper **)
  val last_index  : t -> int

  (** Trim the AdaptiveCounter given a priority **)
  val trim        : t -> int -> t

  (** Compare two AdaptiveCounters with each other,
      returning -1 if first is smaller, 0 if they're
      equal and 1 if the first is bigger **)
  val compare     : t -> t -> int

  (** Append a BString to the end of the AdaptiveCounter **)
  val append      : t -> BString.t -> t

  (** Get last item of the AdaptiveCounter **)
  val get_last    : t -> BString.t

  (** Remove last item of the AdaptiveCounter **)
  val remove_last : t -> t

  (** Trim the AdaptiveCounter to last non-empty BString **)
  val trim_to_last_nonempty : t -> t

  (** Set the BString at specified index **)
  val set         : t -> int -> BString.t -> unit

  (** Set the last BString of the AdaptiveCounter **)
  val set_last    : t -> BString.t -> unit

  (** Determine if the AdaptiveCounter is the top element **)
  val is_max      : t -> bool

  (** Determine if the AdaptiveCounter is empty **)
  val is_empty    : t -> bool

  (** Returns the string representation of the AdaptiveCounter **)
  val show        : t -> string

  (** Prints the AdaptiveCounter to the standard output **)
  val print       : t -> unit
end

module ProgressMeasure : sig
  type t
  (** Smallest even value greater than or equal to the
      max priority in the game **)
  val d : int ref

  (** Number of nodes with odd priority **)
  val mu : int ref

  (** Size of the ProgressMeasure i.e. the number of nodes **)
  val size : t -> int

  (** Returns the string representation of the ProgressMeasure **)
  val show : t -> string

  (** Create ProgressMeasure mapping all nodes to the
      lowest possible AdaptiveCounter **)
  val create               : Paritygame.paritygame -> Paritygame.nodeset -> t

  (** Lift method for making the given node progressive **)
  val lift                 : t -> Paritygame.paritygame -> Paritygame.node -> unit

  (** Helper lift method for determining the smallest
      AdaptiveCounter making the edge progressive **)
  val lift_                : t -> Paritygame.paritygame -> Paritygame.node -> Paritygame.node -> AdaptiveCounter.t

  (** Get AdaptiveCounter for the given node **)
  val get_AC               : t -> Paritygame.node -> AdaptiveCounter.t

  (** Check if a given edge is progressive. Returns
      an error if edge doesn't exist **)
  val is_edge_progressive  : t -> Paritygame.paritygame -> Paritygame.node -> Paritygame.node -> bool

  (** Check if a given node is progressive **)
  val is_node_progressive  : t -> Paritygame.paritygame -> Paritygame.node -> bool

  (** Get the winning set for the given ProgressMeasure **)
  val get_winning_set      : t -> Paritygame.paritygame -> Paritygame.solution

  (** Get the winning strategy for the given ProgressMeasure **)
  val get_winning_strategy : t -> Paritygame.paritygame -> Paritygame.nodeset -> Paritygame.strategy
end