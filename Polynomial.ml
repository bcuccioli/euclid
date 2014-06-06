module Polynomial = struct

  (* Terms have a rational coefficient and variables with integer powers. *)
  type term = (int * int) * (string * int) list
  type poly = term list

  module Order = struct
    type order = int list -> int list -> int
    type porder = term -> term -> int
    module SS = Set.Make(String)

    let sum = List.fold_left (+) 0

    let lex : order = fun a b ->
      let rec leftmost_pos : int list -> int = function
        | h::t -> if h = 0 then leftmost_pos t else (if h > 0 then 1 else -1)
        | []   -> 0 in
      leftmost_pos (List.map2 (fun a b -> a - b) a b)

    let grlex : order = fun a b ->
      if sum a = sum b then lex a b
      else (if sum a > sum b then 1 else -1)

    let grevlex : order = fun a b ->
      if sum a = sum b then -lex (List.rev a) (List.rev b)
      else (if sum a > sum b then 1 else -1)

    let poly_order (f: order) : porder =
      (* TODO: this could be more efficient. *)
      let insert l init = List.fold_left (fun acc x -> SS.add x acc) init l in
      let projv = List.map (fun (v,_) -> v) in
      let lift l m : int list = List.map
        (fun v -> try List.assoc v l with Not_found -> 0)
        (List.sort
          String.compare
          (SS.elements (insert (projv l) (insert (projv m) SS.empty)))) in
      fun (_,l) (_,m) -> f (lift l m) (lift m l)
  end

  let to_string (p: poly) : string =
    let string_of_term ((m,n),l) =
      let string_of_var (v,p) =
        (if p = 0 then "" else v) ^
        (if p > 1 then "^" ^ (string_of_int p) else "") in
      let cs =
        if m mod n = 0 then (if m/n = 1 then "" else (string_of_int (m/n)))
        else "(" ^ (string_of_int m) ^ "/" ^ (string_of_int n) ^ ")" in
      cs ^ String.concat "" (List.map string_of_var l) in
    String.concat "+" (List.map string_of_term p)

  let to_poly (p: int list) : poly =
    List.mapi (fun i t -> ((t,1),["x",i])) p

end
