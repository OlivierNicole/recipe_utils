type t

val equal : t -> t -> bool
val compare : t -> t -> int

val nothing : t

val quantity : t -> Fractional.t
val unit : t -> Unit.t

val parser : t Angstrom.t

val pp : Format.formatter -> t -> unit

module Op : sig
  val (+) : t -> t -> t
end
