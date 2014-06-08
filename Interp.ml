module Interp = struct

  type var = string

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

end
