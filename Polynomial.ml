open Rat
open Util

module Polynomial = struct

  module SS = Set.Make(String)

  (* Terms have a rational coefficient and variables with integer powers. *)
  type term = (Rat.t) * (string * int) list
  type poly = term list

  (* Ensures that t has all the same variables present as t union s. *)
  let normalize_world (ps: poly list) : poly list =
    let insert_poly (init: SS.t) (p: poly) : SS.t =
      let insert_term (init: SS.t) ((_,t): term) : SS.t =
        List.fold_left (fun acc (v,_) -> SS.add v acc) init t in
      List.fold_left insert_term init p in
    let union = SS.elements (List.fold_left insert_poly SS.empty ps) in
    List.map
      (fun p -> List.map
        (fun (c,t) ->
          (c, List.rev (List.fold_left
            (fun acc v -> (v,try List.assoc v t with Not_found -> 0)::acc)
            [] union))) p) ps

  let simplify ((m,n): Rat.t) : Rat.t =
    let rec gcd (a: Rat.s) (b: Rat.s) : Rat.s =
      if b = Rat.zero then a else gcd b (Rat.rem a b) in
    let g = gcd m n in
    (Rat.div m g, Rat.div n g)

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
      let ((_,s'), (_,t')) = (*normalize_mutual s t*) (s,t) in
      let proj = List.map (fun (_,p) -> p) in
      f (proj s') (proj t')
  end

  let to_string (p: poly) : string =
    let string_of_term ((m,n),l) =
      let string_of_var ((v,p): string*int) : string =
        (if p = 0 then "" else "(" ^ v ^ ")") ^
        (if p > 1 then "^" ^ (string_of_int p) else "") in
      let cs =
        if Rat.rem m n = Rat.zero then Rat.to_string (Rat.div m n)
        else "(" ^ (Rat.to_string m) ^ "/" ^ (Rat.to_string n) ^ ")" in
      cs ^ String.concat "" (List.map string_of_var l) in
    String.concat "+" (List.map string_of_term p)

  let leading_term (p: poly) : term =
    try Util.list_max (Order.poly_order Order.grlex) p
    with Util.Empty_list -> ((Rat.zero,Rat.one), [])

  let scalar_multiply (f: poly) ((p,q): Rat.t) : poly =
    List.map (fun ((m,n),l) -> ((Rat.mul m p,Rat.mul n q), l)) f

  (* Computes a quotient of two monomial terms t/s. *)
  let divide_term (((mt,nt),lt): term) (((ms,ns),ls): term) : term =
    ((Rat.mul mt ns, Rat.mul nt ms),
      (List.map2 (fun (v,p) (_,p') -> (v,p-p')) lt ls))

  let term_times (t: term) (p: poly) : poly =
    let ttimes (((m,n),t): term) (((m',n'),t'): term) : term =
      ((Rat.mul m m', Rat.mul n n'), List.map2 (fun (v,p) (_,p') -> (v,p+p')) t t') in
    List.map (ttimes t) p

  let clean_poly (p: poly) : poly =
    (* Removes terms with 0 coefficient. *)
    List.filter (fun ((m,_),_) -> m <> Rat.zero) p

  let add (m,n) (p,q) =
    (Rat.add (Rat.mul m q) (Rat.mul n p),
    Rat.mul n q)

  let add_term_poly (p: poly) ((c,t): term) : poly =
    let (t', p') = List.partition (fun (_,s) -> t = s) p in
    let (c', t') = match t' with
      | []         -> ((Rat.zero,Rat.one), t)
      | [h]        -> h
      | (hc,ht)::l -> (List.fold_left (fun acc (x,_) -> add acc x) hc l, ht) in
    (add c c', t')::p'

  let add_poly (f: poly) (g: poly) : poly =
    clean_poly (List.fold_left add_term_poly f g)

  let sub_poly (f: poly) (g: poly) : poly =
    let g' = List.map (fun ((m,n),l) -> ((Rat.neg m,n),l)) g in
    clean_poly (add_poly f g')

  let s_poly (f: poly) (g: poly) : poly =
    let (ltf, ltg) = (leading_term f, leading_term g) in
    let gamma = (fun ((c,t): term) ->
      (c, List.map2
        (fun (v,_) y -> (v,y)) t
        (List.map2 max
          (List.map (fun (_,p) -> p) ((fun (_,x) -> x) ltf))
          (List.map (fun (_,p) -> p) ((fun (_,x) -> x) ltg))))) ltf in
    sub_poly
      (term_times (divide_term gamma ltf) f)
      (term_times (divide_term gamma ltg) g)

  let divides ((_,t): term) ((_,s): term) : bool =
    List.for_all2 (fun (_,p) (_,q) -> p <= q) t s

  let is_zero (p: poly) : bool =
    let zero (m,_) = m = Rat.zero in
    let ht = Hashtbl.create (List.length p) in
    List.iter (fun (c,t) -> Hashtbl.add ht t c) p;
    List.for_all
      (fun (_,t) -> zero
        (List.fold_left add (Rat.zero,Rat.one) (Hashtbl.find_all ht t)))
      p

  let modulo (p: poly) (fs: poly list) : poly =
    let ltfs = List.map (fun fi -> (fi, leading_term fi)) fs in
    let rec divstep' (p: poly) (ltp: term) : 'a list -> bool*poly = function
      | []            -> (false, p)
      | (fi,ltfi)::t  -> (
        if divides ltfi ltp then
          (true, sub_poly p (term_times (divide_term ltp ltfi) fi))
        else divstep' p ltp t) in
    let rec divstep (p: poly) (r: poly) : poly =
      if is_zero p then r
      else (
        let (d,p) = divstep' p (leading_term p) ltfs in
        if d then divstep p r
        else (
          let ltp = leading_term p in
          divstep (sub_poly p [ltp]) (add_poly r [ltp]))) in
    divstep p [((Rat.zero,Rat.one),[])]

end
