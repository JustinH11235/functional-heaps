module IntHeap = MyHeap.Make (Int)

let rec deplete_heap heap =
  if IntHeap.is_empty heap then ()
  else
    let v = IntHeap.top heap in
    let heap = IntHeap.pop heap in
    (print_endline @@ "Popped: "
    ^ match v with Some v -> string_of_int v | None -> "IntHeap.top failed");
    deplete_heap heap

let () =
  (* let heap = List.fold_left IntHeap.push IntHeap.empty [ 1; 2; 3; 4; 5; 6 ] in *)
  let heap = IntHeap.of_list [ 1; 6; 2; 4; 3; 5 ] in
  deplete_heap heap
