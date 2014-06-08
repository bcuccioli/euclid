%{
  open Interp
  open Lexing

  exception ParseError of string

  let parse_error _ =
    let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    let start_line = string_of_int start_pos.pos_lnum in
    let start_char = string_of_int (start_pos.pos_cnum - start_pos.pos_bol) in
    let end_line = string_of_int end_pos.pos_lnum in
    let end_char = string_of_int (end_pos.pos_cnum - end_pos.pos_bol) in
    raise (ParseError (start_line^"."^start_char^"-"^end_line^"."^end_char))
%}

%token LPAREN RPAREN COMMA SEMICOLON EOF
%token PARALLEL PERP COLIN DISTEQ CIRCLE MIDPOINT ANGLEEQ BISECTS DECIDE
%token <string> VAR

%start prog
%type <Interp.ast> prog

%%

stmt:
  | PARALLEL LPAREN VAR VAR COMMA VAR VAR RPAREN
    { Interp.Parallel ($3, $4, $6, $7) }
  | PERP LPAREN VAR VAR COMMA VAR VAR RPAREN
    { Interp.Perp ($3, $4, $6, $7) }
  | COLIN LPAREN VAR COMMA VAR COMMA VAR RPAREN
    { Interp.Colin ($3, $5, $7) }
  | DISTEQ LPAREN VAR VAR COMMA VAR VAR RPAREN
    { Interp.DistEq ($3, $4, $6, $7) }
  | CIRCLE LPAREN VAR COMMA VAR COMMA VAR VAR RPAREN
    { Interp.Circle ($3, $5, $7, $8) }
  | MIDPOINT LPAREN VAR COMMA VAR VAR RPAREN
    { Interp.Midpoint ($3, $5, $6) }
  | ANGLEEQ LPAREN VAR VAR VAR COMMA VAR VAR VAR RPAREN
    { Interp.AngleEq ($3, $4, $5, $7, $8, $9) }
  | BISECTS LPAREN VAR VAR COMMA VAR VAR VAR RPAREN
    { Interp.Bisects ($3, $4, $6, $7, $8) }
;

conclusion:
  | DECIDE stmt { $2 }
;

hyps:
  | { Interp.Nil }
  | stmt SEMICOLON hyps { Interp.Cons ($1, $3) }
;

prog:
  | hyps conclusion { Interp.Program (Interp.Statement $1, Interp.Decide $2) }
;
