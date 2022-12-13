open Core
open Option
(* open Option.Monad_infix *)

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

(* maybe in future we can also parametrize Make with a specific heap module,
   or multiple classes with different properties, like one has O(1) length *)
module Make (Ord : ORDERED_TYPE) = struct
  type elem_t = Ord.t

  type t = t_rec option
  and t_rec = { l : t; v : elem_t; r : t; h : int }

  let ( <: ) a b = Ord.compare a b < 0
  (* let ( >: ) a b = Ord.compare a b > 0 *)

  (* let ( <=: ) a b = Ord.compare a b <= 0 *)
  (* let ( >=: ) a b = Ord.compare a b >= 0 *)
  let height = function None -> 0 | Some { h; _ } -> h
  let value = function None -> None | Some { v; _ } -> Some v

  let create_node ?h l v r =
    Some
      {
        l;
        v;
        r;
        h =
          (match h with
          | None -> succ @@ min (height l) (height r)
          | Some h -> h);
      }

  let update_v node v =
    match node with Some { l; r; h; _ } -> create_node l v r ~h | None -> None

  let update_l node l =
    match node with Some { v; r; h; _ } -> create_node l v r ~h | None -> None

  let update_r node r =
    match node with Some { l; v; h; _ } -> create_node l v r ~h | None -> None

  let rec rem_last heap =
    match heap with
    | Some { l; v; r; _ } -> (
        if is_none l && is_none r then Some (None, v)
        else if height l > height r then
          match rem_last l with
          | Some (l, last) -> Some (update_l heap l, last)
          | None -> None
        else
          match rem_last r with
          | Some (r, last) -> Some (update_r heap r, last)
          | None -> None)
    | None -> None

  (* remove last, update top's data to last *)
  let move_last_to_top heap =
    match rem_last heap with
    | Some (heap, last) -> update_v heap last
    | None -> None

  let swap_l node =
    match node with
    | Some { l; v; r; h } -> (
        match l with
        | Some { v = lv; _ } ->
            if lv <: v then create_node (update_v l v) lv r ~h else node
        | None -> node)
    | None -> None

  let swap_r node =
    match node with
    | Some { l; v; r; h } -> (
        match r with
        | Some { v = rv; _ } ->
            if rv <: v then create_node l rv (update_v r v) ~h else node
        | None -> node)
    | None -> None

  let rec heap_down heap =
    match heap with
    | Some { l; r; _ } -> (
        match (l, r) with
        | Some { v = lv; _ }, Some { v = rv; _ } -> (
            if lv <: rv then
              match swap_l heap with
              | Some { l; _ } as heap -> update_l heap (heap_down l)
              | None -> None
            else
              match swap_r heap with
              | Some { r; _ } as heap -> update_r heap (heap_down r)
              | None -> None)
        | Some _, _ -> (
            match swap_l heap with
            | Some { l; _ } as heap -> update_l heap (heap_down l)
            | None -> None)
        | _, Some _ -> (
            match swap_r heap with
            | Some { r; _ } as heap -> update_r heap (heap_down r)
            | None -> None)
        | _ -> heap)
    | None -> None

  let is_empty = is_none
  let empty = None

  (* traverse to first open node based on smallest h,
     then, as you come back up, swap nodes to maintain heap order*)
  let rec push heap elem =
    match heap with
    | Some { l; r; _ } ->
        if height l <= height r then swap_l (update_l heap (push l elem))
        else swap_r (update_r heap (push r elem))
    | None -> create_node None elem None

  (* traverse to the last filled node based on largest h,
      then, remove the node and as you come up make it the top,
      then, swap nodes from top to maintain heap order*)
  let pop heap = move_last_to_top heap |> heap_down
  let top = value

  let rec length = function
    | Some { l; r; _ } -> length l + length r + 1
    | None -> 0
end
