{
  open Parser

  exception Eof

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }
}

let var = ['A'-'Z']
let whitespace = [' ' '\t']

rule token = parse
  | whitespace  { token lexbuf }
  | ['\n']      { incr_linenum lexbuf; token lexbuf }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | ","         { COMMA }
  | ";"         { SEMICOLON }
  | "parallel"  { PARALLEL }
  | "perp"      { PERP }
  | "colin"     { COLIN }
  | "disteq"    { DISTEQ }
  | "circle"    { CIRCLE }
  | "midpoint"  { MIDPOINT }
  | "angleeq"   { ANGLEEQ }
  | "bisects"   { BISECTS }
  | "decide"    { DECIDE }
  | var as v    { VAR (Char.escaped v) }
  | eof         { EOF }
