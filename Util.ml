module Util = struct

  exception Empty_list

  (* Computes the list maximum according to f in O(n) time and O(1) space. *)
  let list_max (f: 'a -> 'a -> int) : 'a list -> 'a = function
    | []   -> raise Empty_list
    | [h]  -> h
    | h::t ->  List.fold_left (fun acc x -> if f acc x = 1 then acc else x) h t

  (* Computes the list of unique elements according to =. *)
  let unique : 'a list -> 'a list =
    List.fold_left
      (fun acc x -> if List.mem x acc then acc else x::acc)
      []

end
