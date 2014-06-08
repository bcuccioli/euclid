open Util

module Polynomial = struct

  module SS = Set.Make(String)

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

  let normalize_mutual (t: term) (s: term) : term * term =
    (normalize t s, normalize s t)

  let normalize_in_poly (t: term) (p: poly) : term =
    List.fold_left normalize t p

  let normalize_poly (p: poly) (q: poly) : poly =
    List.map (fun t -> normalize_in_poly t q) p

  let simplify ((m,n): int*int) : int*int =
    let rec gcd (a: int) (b: int) : int =
      if b = 0 then a else gcd b (a mod b) in
    let g = gcd m n in
    (m/g, n/g)

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
      let ((_,s'), (_,t')) = normalize_mutual s t in
      let proj = List.map (fun (_,p) -> p) in
      f (proj s') (proj t')
  end

  let to_string (p: poly) : string =
    let string_of_term ((m,n),l) =
      let string_of_var (v,p) =
        (if p = 0 then "" else v) ^
        (if p > 1 then "^" ^ (string_of_int p) else "") in
      let cs =
        if m mod n = 0 then string_of_int (m/n)
        else "(" ^ (string_of_int m) ^ "/" ^ (string_of_int n) ^ ")" in
      cs ^ String.concat "" (List.map string_of_var l) in
    String.concat "+" (List.map string_of_term p)

  let to_poly (p: int list) : poly =
    List.mapi (fun i t -> ((t,1),["x",i])) p

  let leading_term : poly -> term =
    Util.list_max (Order.poly_order Order.grlex)

  let multideg (p: poly) (g: poly) : int list =
    let t = normalize_in_poly (leading_term p) p in
    let (_,l) = normalize_in_poly t g in
    List.map (fun (_,p) -> p) l

  let scalar_multiply (f: poly) ((p,q): int*int) : poly =
    List.map (fun ((m,n),l) -> (simplify (m*p,n*q), l)) f

  (* Computes a quotient of two monomial terms t/s. *)
  let divide_term (t: term) (s: term) : term =
    let (((mt,nt),lt), ((ms,ns),ls)) = normalize_mutual t s in
    (simplify (mt*ns, nt*ms), (List.map2 (fun (v,p) (_,p') -> (v,p-p')) lt ls))

  let term_times (t: term) (p: poly) : poly =
    let ttimes (((m,n),t): term) (((m',n'),t'): term) : term =
      (* Assumes terms are normalized. *)
      (simplify (m*m', n*n'), List.map2 (fun (v,p) (_,p') -> (v,p+p')) t t') in
    List.map (fun s -> let (t,s) = normalize_mutual t s in ttimes t s) p

  let clean_poly (p: poly) : poly =
    (* Removes terms with 0 coefficient, factors with 0 power, and simplifies
     * coefficients. *)
    List.map
      (fun (c,l) -> (simplify c, List.filter (fun (_,p) -> p <> 0) l))
      (List.filter (fun ((m,_),_) -> m <> 0) p)

  let add_term_poly (p: poly) ((c,t): term) : poly =
    let add (m,n) (p,q) = (m*q + n*p, n*q) in
    let (t', p') = List.partition (fun (_,s) -> t = s) p in
    let (c', t') = match t' with
      | []         -> ((0,1), t)
      | [h]        -> h
      | (hc,ht)::l -> (List.fold_left (fun acc (x,_) -> add acc x) hc l, ht) in
    (add c c', t')::p'

  let add_poly (f: poly) (g: poly) : poly =
    clean_poly (List.fold_left add_term_poly f g)

  let sub_poly (f: poly) (g: poly) : poly =
    let g' = List.map (fun ((m,n),l) -> ((-m,n),l)) g in
    clean_poly (add_poly f g')

  let s_poly (f: poly) (g: poly) : poly =
    (* TODO: we could decrease the total number of calls to normalize here. *)
    let f = normalize_poly f g in
    let g = normalize_poly g f in
    let (ltf, ltg) = (leading_term f, leading_term g) in
    let (ltf, ltg) = (normalize_in_poly ltf g, normalize_in_poly ltg f) in
    let gamma = (fun ((c,t): term) ->
      (c, List.map2
        (fun (v,_) y -> (v,y)) t
        (List.map2 max (multideg f g) (multideg g f)))) ltf in
    sub_poly
      (term_times (divide_term gamma ltf) f)
      (term_times (divide_term gamma ltg) g)

  let divides (t: term) (s: term) : bool =
    let ((_,t), (_,s)) = normalize_mutual t s in
    List.for_all2 (fun (_,p) (_,q) -> p <= q) t s

  let modulo (p: poly) (fs: poly list) : poly =
    let rec divstep' (p: poly) (i: int) : bool*poly =
      if i >= List.length fs then (false, p)
      else (
        let fi = List.nth fs i in
        if divides (leading_term fi) (leading_term p) then
          (true, sub_poly p
            (term_times (divide_term (leading_term p) (leading_term fi)) fi))
        else divstep' p (i+1)) in
    let rec divstep (p: poly) (r: poly) : poly =
      if p = [] then r
      else (
        let (d,p) = divstep' p 0 in
        if d then divstep p r
        else divstep
          (sub_poly p [leading_term p])
          (add_poly r [leading_term p])) in
    divstep p [((0,1),[])]

  let is_zero (p: poly) : bool =
    (* TODO: this is not quite right - cancellation can occur. *)
    List.for_all (fun ((m,_),_) -> m = 0) p

end
