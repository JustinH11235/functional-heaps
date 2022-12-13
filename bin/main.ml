module IntHeap = MyHeap.Make (Int)

let () =
  let heap =
    List.fold_left IntHeap.push IntHeap.empty [ 1; 67; 4; 3; 6; 34; 6 ]
  in
  let heap = IntHeap.pop heap in
  (print_endline
  @@ match IntHeap.top heap with None -> "" | Some v -> string_of_int v);
  print_endline @@ string_of_int @@ IntHeap.length heap
