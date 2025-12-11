type t

val int : int -> t
val frac : int -> int -> t
val float : float -> t

val zero : t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_float : t -> float

module Op : sig
  val (+) : t -> t -> t
end
