module Polynomial = struct

  module SS = Set.Make(String)

  exception Empty_polynomial

  (* Terms have a rational coefficient and variables with integer powers. *)
  type term = (int * int) * (string * int) list
  type poly = term list

  (* Ensures that t has all the same variables present as t union s. *)
  let normalize ((ct,lt): term) ((cs,ls): term) : term =
    (* TODO: this could be more efficient. *)
    let lift l m : (string * int) list =
      let insert l init = List.fold_left (fun acc x -> SS.add x acc) init l in
      let proj = List.map (fun (v,_) -> v) in
      List.map
        (fun v -> (v, try List.assoc v l with Not_found -> 0))
        (List.sort
          String.compare
          (SS.elements (insert (proj l) (insert (proj m) SS.empty)))) in
    (ct, lift lt ls)

  let normalize_in_poly (t: term) (p: poly) : term =
    List.fold_left normalize t p

  module Order = struct
    type order = int list -> int list -> int
    type porder = term -> term -> int

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

    let poly_order (f: order) : porder = fun s t ->
      let (_,s') = normalize s t in
      let (_,t') = normalize t s in
      let proj = List.map (fun (_,p) -> p) in
      f (proj s') (proj t')
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
