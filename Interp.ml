open Polynomial.Polynomial
open Rat

module Interp = struct

  type var = string
  type point = string

  type stmt =
    | Parallel of var * var * var * var
    | Perp of var * var * var * var
    | Colin of var * var * var
    | DistEq of var * var * var * var
    | AngleEq of var * var * var * var * var * var

  type ast =
    | Program of stmt list * stmt

  let rec stmt_to_string : stmt -> string = function
    | Parallel (a,b,c,d)    -> Printf.sprintf "Parallel (%s%s,%s%s)" a b c d
    | Perp (a,b,c,d)        -> Printf.sprintf "Perp (%s%s,%s%s)" a b c d
    | Colin (a,b,c)         -> Printf.sprintf "Colin (%s,%s,%s)" a b c
    | DistEq (a,b,c,d)      -> Printf.sprintf "DistEq (%s%s,%s%s)" a b c d
    | AngleEq (a,b,c,d,e,f) ->
        Printf.sprintf "AngleEq (%s%s%s,%s%s%s)" a b c d e f

  let rec to_string : ast -> string = function
    | Program (s,t) ->
        (String.concat "" (List.map (stmt_to_string) s)) ^
        (stmt_to_string t)

  let ast_to_poly (ast: ast) : poly list * poly list =
    let vars = ref [] in
    let hyps = ref [] in
    let conc = ref [] in
    let f (v,w) coef =
      ((coef,1),[(v,1);(w,1)]) in
    let add_var (v: var) : point*point =
      if List.exists (fun (s,_) -> s = v) !vars then
        let (_,p) = List.find (fun (s,_) -> s = v) !vars in p
      else (
        let (_,next) = match !vars with
        | []  -> (v,("0","0"))
        | [h] -> (v,("0","x1"))
        | _   -> (
          let n = List.length !vars in
          let (x,y) = (string_of_int (n*2), string_of_int (n*2+1)) in
          (v,("x"^x, "x"^y))) in
        vars := (v,next)::(!vars);
        next) in
    let rec poly_of_stmt = function
      | Parallel (a,b,c,d) -> (
        let ((ax,ay),(bx,by),(cx,cy),(dx,dy)) =
          (add_var a, add_var b, add_var c, add_var d) in
        (f (cy,dx) 1)::(f (cy,bx) (-1))::(f (ay,dx) (-1))::(f (ay,bx) 1)::
          (f (dy,cx) (-1))::(f (dy,ax) 1)::(f (by,cx) 1)::[f (by,ax) (-1)])
      | Colin (a,b,c) -> (
        let ((ax,ay),(bx,by),(cx,cy)) = (add_var a, add_var b, add_var c) in
        (f (by,cx) 1)::(f (by,ax) (-1))::(f (ay,cx) (-1))::(f (cy,bx) (-1))::
          (f (cy,ax) 1)::[(f (ay,bx) 1)])
      | DistEq (a,b,c,d) -> (
        let ((ax,ay),(bx,by),(cx,cy),(dx,dy)) =
          (add_var a, add_var b, add_var c, add_var d) in
        [(1,1),[ay,2]; (1,1),[by,2]; f (ay,by) (-2);
          (1,1),[ax,2]; (1,1),[bx,2]; f (ax,bx) (-2);
          (-1,1),[cy,2]; (-1,1),[dy,2]; f (cy,dy) 2;
          (-1,1),[cx,2]; (-1,1),[dx,2]; f (cx,dx) 2])
      | Perp (a,b,c,d) -> (
        let ((ax,ay),(bx,by),(cx,cy),(dx,dy)) =
          (add_var a, add_var b, add_var c, add_var d) in
        (f (by,dy) 1)::(f (by,cy) (-1))::(f (ay,dy) (-1))::(f (ay,cy) 1)::
          (f (bx,cx) (-1))::(f (bx,dx) 1)::(f (ax,cx) 1)::[(f (ax,dx) (-1))])
      | AngleEq (a,b,c,d,e,f) -> (
        (* TODO: Encode this as a polynomial. AngleEq is true iff
         * (BC)(DF) = (AC)(EF). *)
        failwith "not implemented.") in
    let clean_poly_of_stmt (s: stmt) : poly =
      (* Cleans out terms with 0 factors that arise because one "variable"
       * is 0. *)
      let p = poly_of_stmt s in
      let p = List.map (fun ((m,n),t) -> ((Rat.of_int m,Rat.of_int n),t)) p in
      List.filter (fun (_,t) -> List.for_all (fun (v,_) -> v <> "0") t) p in
    let morph_concs (concs: poly list) : poly list =
      let prods =
        List.map
          (fun p -> List.map (fun ((m,n),t) -> ((Rat.neg m,n),t@["y",0])) p)
          concs in
      List.map (add_poly [((Rat.one,Rat.one),[])]) prods in
    match ast with
      | Program (s,t) -> (
        hyps := List.map clean_poly_of_stmt s;
        conc := morph_concs (List.map clean_poly_of_stmt [t]);
        (!hyps, !conc))

end
