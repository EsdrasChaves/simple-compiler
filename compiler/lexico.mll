{
    open Lexing
    open Printf
    type tokens = 
        | LITINT of int
        | LITREAL of float
        | LITSTRING of string
        | LITBOOL of bool
        | ID of string
        | MAIS | MENOS | ASTER | DBARRA | IGUAL
        | MENOR | MAIOR
        | NAOIGUAL
        | ACHAV | FCHAV
        | APAR | FPAR
        | ACOLCH | FCOLCH
        | PONTO | VIRG | DPONTOS | PVIRG
        | ACIRC
        | ARROBA
        | DOLAR
        | CERQU
        | MENORIGUAL | MAIORIGUAL
        | MENOSIGUAL | MAISIGUAL | MULTIGUAL | DIVIGUAL
        | E | OU | NOT
        | ATRIB
        | PROGRAM
        | BEGIN
        | END
        | VAR | CONST
        | STRING | INTEGER | REAL | BOOLEAN
        | IF | THEN | ELSE 
        | FOR | TO | DOWNTO | DO | WHILE | CASE | OF | REPEAT | UNTIL
        | PROCEDURE
        | EOF


    let incr_num_linha lexbuf =
        let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { pos with
                pos_lnum = pos.pos_lnum + 1;
                pos_bol = pos.pos_cnum;
            }

    let msg_erro lexbuf c =
        let pos = lexbuf.lex_curr_p in
        let lin = pos.pos_lnum
        and col = pos.pos_cnum - pos.pos_bol - 1 in
        sprintf "%d-%d: caracter desconhecido %c" lin col c

    let erro lin col msg =
        let mensagem = sprintf "%d-%d: %s" lin col msg in           
            failwith mensagem
}

let digito = [ '0' - '9' ]
let inteiro = digito+
let real = digito+"." digito*
let verdade = "true"
let falso = "false"
let letra = [ 'a' - 'z' 'A' - 'Z' ]
let identificador = letra ( letra | digito | '_' )*
let brancos = [ ' ' '\t' ]+
let novalinha = '\r' | '\n' | "\r\n"

rule token = parse
    brancos { token lexbuf }
    | novalinha { incr_num_linha lexbuf; token lexbuf }
    | '{' { comentario_in_line 0 lexbuf }
    | "{*" { comentario_bloco 0 lexbuf }
    | '+' { MAIS }
    | '-' { MENOS }
    | '*' { ASTER }
    | '/' { DBARRA }
    | '=' { IGUAL }
    | '<' { MENOR }
    | '>' { MAIOR }
    | '[' { ACOLCH }
    | ']' { FCOLCH }
    | '(' { APAR }
    | ')' { FPAR }
    | '.' { PONTO }
    | ',' { VIRG }
    | ':' { DPONTOS }
    | ';' { PVIRG }
    | '^' { ACIRC }
    | '@' { ARROBA }
    | '$' { DOLAR }
    | '#' { CERQU }
    | "<>" { NAOIGUAL }
    | "<=" { MENORIGUAL }
    | ">=" { MAIORIGUAL }
    | "-=" { MENOSIGUAL }
    | "+=" { MAISIGUAL }
    | "*=" { MULTIGUAL }
    | "/=" { DIVIGUAL }
    | "and" { E }
    | "or" { OU }
    | "not" { NOT }
    | ":=" { ATRIB }
    | "program" { PROGRAM }
    | "begin" { BEGIN }
    | "end" { END }
    | "var" { VAR }
    | "const" { CONST }
    | "string" { STRING }
    | "integer" { INTEGER }
    | "real" { REAL }
    | "boolean" { BOOLEAN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "for" { FOR }
    | "to" { TO }
    | "downto" { DOWNTO }
    | "do" { DO }
    | "while" { WHILE }
    | "case" { CASE }
    | "of" { OF }
    | "repeat" { REPEAT }
    | "until" { UNTIL }
    | "procedure" { PROCEDURE }
    | inteiro as num { let numero = int_of_string num in LITINT numero }
    | real as num { let numero = float_of_string num in LITREAL numero}
    | verdade as btrue { let boolean = bool_of_string btrue in LITBOOL boolean}
    | falso as bfalse { let boolean = bool_of_string bfalse in LITBOOL boolean}
    | identificador as id { ID id }
    | '''     { let pos = lexbuf.lex_curr_p in
                    let lin = pos.pos_lnum
                    and col = pos.pos_cnum - pos.pos_bol - 1 in
                    let buffer = Buffer.create 1 in
                    let str = leia_string lin col buffer lexbuf in
                        LITSTRING str }
    | _ as c { failwith (msg_erro lexbuf c) }
    | eof { EOF }

    and leia_string lin col buffer = parse
          '''  { Buffer.contents buffer}
        | "\\t"  { Buffer.add_char buffer '\t' ;
                    leia_string lin col buffer lexbuf }
        | "\\n"  { Buffer.add_char buffer '\n' ;
                    leia_string lin col buffer lexbuf }
        | '\\' ''' { Buffer.add_char buffer '"' ;
                    leia_string lin col buffer lexbuf }
        | '\\' '\\' { Buffer.add_char buffer '\\' ;
                    leia_string lin col buffer lexbuf }
        | _ as c { Buffer.add_char buffer c;
                    leia_string lin col buffer lexbuf }
        | eof  { erro lin col "A string não foi fechada"}

    and comentario_bloco n = parse
        "*}" { if n=0 then token lexbuf
                else comentario_bloco (n-1) lexbuf }
        | "{*"  { comentario_bloco (n+1) lexbuf }
        | novalinha { incr_num_linha lexbuf;
                    comentario_bloco n lexbuf }
        | _  { comentario_bloco n lexbuf }
        | eof { failwith "Comentário de bloco não fechado" }

    and comentario_in_line n = parse
        '}' { if n=0 then token lexbuf
                else comentario_in_line (n-1) lexbuf }
        | '{'  { comentario_in_line (n+1) lexbuf }
        | novalinha { failwith "Comentário de linha não fechado" }
        | _  { comentario_in_line n lexbuf }
        | eof { failwith "Comentário de linha não fechado" }