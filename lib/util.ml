(** Removes element at a given index and returns the result. *)
let rm_el lst index =
  let rec aux acc idx = function
    | [] -> List.rev acc
    | _ :: tail when idx == index -> List.rev_append acc tail
    | a :: tail -> aux (a :: acc) (idx + 1) tail
  in
  aux [] 0 lst
