open Ideal
open Interp
open Parser
open Polynomial
open Lexing

let print_poly_list (ps: Polynomial.poly list) : unit =
  List.iter
    (fun s -> print_endline (Polynomial.to_string s))
    ps

let () =
  let chan =
    if Array.length Sys.argv < 2 then stdin
    else (open_in Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel chan in
  let ast = prog Lexer.token lexbuf in
  let (hyps, concs) = Interp.ast_to_poly ast in

  Printf.printf "Judgment: %s\n"
    (if Ideal.trivial_basis (hyps, concs) then "yes" else "no");

  close_in chan
