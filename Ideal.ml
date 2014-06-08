open Polynomial
open Util

module Ideal = struct

  type ideal = Polynomial.poly list

  let basis (fs: ideal) : Polynomial.poly list =
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
    Util.unique (step fs)

end
