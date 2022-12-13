module type ORDERED_TYPE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem_t
  type t

  val is_empty : t -> bool
  val empty : t

  (* val of_list : elem_t list -> t *)
  val push : t -> elem_t -> t
  val pop : t -> t
  val top : t -> elem_t option
  val length : t -> int
end

module Make (Ord : ORDERED_TYPE) : HEAP with type elem_t = Ord.t
