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