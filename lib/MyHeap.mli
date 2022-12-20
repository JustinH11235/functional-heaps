module type ORDERED_TYPE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem_t
  type t

  val is_empty : t -> bool
  val empty : t
  val of_list : elem_t list -> t
  val to_list : t -> elem_t list (* O(n) *)
  val to_list_ordered : t -> elem_t list (* O(nlogn) *)
  val push : t -> elem_t -> t
  val pop : t -> t
  val top : t -> elem_t option
  val top_exn : t -> elem_t (* O(1) throws invalid arg if empty *)
  val length : t -> int

  val mem :
    elem_t -> t -> bool (* O(n) worst case, optimized to stop searching early*)
end

module Make (Ord : ORDERED_TYPE) : HEAP with type elem_t = Ord.t
