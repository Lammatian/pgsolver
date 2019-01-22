(*open Paritygame

val solve : paritygame -> solution * strategy
val register : unit -> unit
*)

val solve : Paritygame.paritygame -> Paritygame.solution * Paritygame.strategy
val register: unit -> unit

module BString : sig
  type t
  (** Create BString from a list of booleans **)
  val create    : bool list -> t
  (** Create BString of all 0s of length of the first argument **)
  val createLen : int -> t
  (** Returns the string representation of the BString **)
  val show      : t -> string
  (** Prints the BString to the standard output **)
  val print     : t -> unit
  (** Appends 10...0 to a total length of the second argument **)
  val append    : t -> int -> t
  (** Cut the last 01...1 from the BString **)
  val cut       : t -> t
  (** Get the length of the bstring **)
  val length    : t -> int
  (** Compare two BStrings with each other, returning
      -1 if first is smaller, 0 if they're equal and
      1 if first is bigger than the second one **)
  val compare   : t -> t -> int
end

module AdaptiveCounter : sig
  type t
  (** Create AdaptiveCounter from list of BStrings **)
  val create    : BString.t list -> t
  (** Create Top AdaptiveCounter **)
  val createTop : unit -> t
  (** Get length of the AdaptiveCounter defined as
      the number of BStrings inside it **)
  val length    : t -> int
  (** Trim the AdaptiveCounter of the last n entries **)
  val trim      : t -> int -> t
  (** Compare two AdaptiveCounters with each other,
      returning -1 if first is smaller, 0 if they're
      equal and 1 if the first is bigger **)
  val compare   : t -> t -> int
  (** Append a BString to the end of the AdaptiveCounter **)
  val append    : t -> BString.t -> t
  (** Get last item of the AdaptiveCounter **)
  val getLast   : t -> BString.t
  (** Determine if the AdaptiveCounter is the top element **)
  val isMax     : t -> bool
  (** Returns the string representation of the AdaptiveCounter **)
  val show      : t -> string
  (** Prints the BString to the standard output **)
  val print     : t -> unit
end

module ProgressMeasure : sig
  type t
  (** Create ProgressMeasure mapping all nodes to
      the lowest possible AdaptiveCounter **)
  val create             : Paritygame.nodeset -> t
  (** Lift method for making the given node progressive **)
  val lift               : t -> Paritygame.node -> unit
  (** Get AdaptiveCounter for the given node **)
  val getAC              : t -> Paritygame.node -> AdaptiveCounter.t
  (** Get the winning set for the given ProgressMeasure **)
  val getWinningSet      : t -> Paritygame.solution
  (** Get the winning strategy for the given ProgressMeasure **)
  val getWinningStrategy : t -> Paritygame.strategy
end