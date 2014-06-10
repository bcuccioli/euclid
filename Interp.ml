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
    | Circle of var * var * var * var
    | Midpoint of var * var * var
    | AngleEq of var * var * var * var * var * var
    | Bisects of var * var * var * var * var
    | Cons of stmt * stmt
    | Nil

  type ast =
    | Statement of stmt
    | Decide of stmt
    | Program of ast * ast

  let rec stmt_to_string : stmt -> string = function
    | Parallel (a,b,c,d)    -> Printf.sprintf "Parallel (%s%s,%s%s)" a b c d
    | Perp (a,b,c,d)        -> Printf.sprintf "Perp (%s%s,%s%s)" a b c d
    | Colin (a,b,c)         -> Printf.sprintf "Colin (%s,%s,%s)" a b c
    | DistEq (a,b,c,d)      -> Printf.sprintf "DistEq (%s%s,%s%s)" a b c d
    | Circle (a,b,c,d)      -> Printf.sprintf "Circle (%s,%s,%s%s)" a b c d
    | Midpoint (a,b,c)      -> Printf.sprintf "Midpoint (%s,%s,%s)" a b c
    | AngleEq (a,b,c,d,e,f) ->
        Printf.sprintf "AngleEq (%s%s%s,%s%s%s)" a b c d e f
    | Bisects (a,b,c,d,e)   -> Printf.sprintf "Bisects (%s%s,%s%s%s)" a b c d e
    | Cons (s,t)            -> (stmt_to_string s) ^ "\n" ^ (stmt_to_string t)
    | Nil                   -> ""

  let rec to_string : ast -> string = function
    | Program (s,t) -> (to_string s) ^ (to_string t)
    | Statement s   -> "Hypothesis: " ^ (stmt_to_string s) ^ "\n"
    | Decide s      -> "Decide: " ^ (stmt_to_string s) ^ "\n"

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
      | Nil | Cons _ -> failwith "Error: desugar first."
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
      | _ -> failwith "not yet implemented" in
    let clean_poly_of_stmt (s: stmt) : poly =
      (* Cleans out terms with 0 factors that arise because one "variable"
       * is 0. *)
      let p = poly_of_stmt s in
      let p = List.map (fun ((m,n),t) -> ((Rat.of_int m,Rat.of_int n),t)) p in
      List.filter (fun (_,t) -> List.for_all (fun (v,_) -> v <> "0") t) p in
    let rec desugar_stmts (l: stmt list) : stmt -> stmt list = function
      | Nil         -> l
      | Cons (a,b)  -> desugar_stmts (a::l) b
      | _ as s      -> s::l in
    let morph_concs (concs: poly list) : poly list =
      let prods =
        List.map
          (fun p -> List.map (fun ((m,n),t) -> ((Rat.neg m,n),t@["y",0])) p)
          concs in
      List.map (add_poly [((Rat.one,Rat.one),[])]) prods in
    match ast with
      | Program (Statement s, Decide t) -> (
        hyps := List.map clean_poly_of_stmt (desugar_stmts [] s);
        conc := morph_concs (List.map clean_poly_of_stmt (desugar_stmts [] t));
        (!hyps, !conc))
      | _             -> failwith "Parser error: Did not produce Program."

end
