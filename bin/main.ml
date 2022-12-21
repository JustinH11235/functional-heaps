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
  let heap = IntHeap.of_list [ 1; 4; 2; 3; 5 ] in
  let heap = IntHeap.push heap 6 in
  let heap = IntHeap.push heap 2 in
  deplete_heap heap
