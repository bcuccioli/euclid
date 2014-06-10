open Polynomial
open Rat
open Util

module Ideal = struct

  type ideal = Polynomial.poly list

  let basis (fs: ideal) : Polynomial.poly list =
    let fs = Polynomial.normalize_world fs in
    let pairs (l: 'a list) : ('a * 'a) list =
      let pairify (a: 'a) (l: 'a list) : ('a * 'a) list =
        List.fold_left (fun ac x -> if x <> a then (x,a)::ac else ac) [] l in
      List.fold_left (fun acc x -> (pairify x l)@acc) [] l in
    let join g (l: Polynomial.poly list) (p,q) : ideal =
      let s = Polynomial.modulo (Polynomial.s_poly p q) g in
      if not (Polynomial.is_zero s) then s::l else l in
    let rec step g =
      let g_new = List.fold_left (join g) g (pairs g) in
      if g_new = g then g_new
      else step g_new in
    step fs

  (* A reduced basis for a polynomial ideal is a basis G such that:
    * each leading term has coefficient 1; and
    * no monomial of p lies in <LT(G-{p})>.
  *)
  let reduced_basis (fs: ideal) : Polynomial.poly list =
    let open Polynomial in
    let scale_leading (p: poly) : poly =
      let ((m,n),_) = Polynomial.leading_term p in
      scalar_multiply p (n,m) in
    let is_redundant (p: poly) (terms: (term*poly) list) : bool =
      let lt = leading_term p in
      List.exists (fun (t,_) -> lt <> t && divides t lt) terms in
    let unique_neg : poly list -> poly list =
      List.fold_left
        (fun acc x ->
          if List.mem x acc ||
            List.mem (Polynomial.scalar_multiply x
              (Rat.neg Rat.one, Rat.one)) acc
          then acc else x::acc)
        [] in
    let l = List.map scale_leading (unique_neg (basis fs)) in
    let leads = List.map (fun x -> (leading_term x, x)) l in
    List.filter (fun p -> not (is_redundant p leads)) l

  let trivial_basis (hyps,conc) : bool =
    let rb = List.map
      (fun p -> (List.map
        (fun (c,t) -> (Polynomial.simplify c,
          List.filter (fun (_,p) -> p <> 0) t)) p))
      (reduced_basis (conc@hyps)) in
    rb = [] || rb = [[((Rat.one,Rat.one),[])]]

end
