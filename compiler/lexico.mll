{
    open Lexing
    open Printf
	open Sintatico

    exception Erro of string

    let incr_num_linha lexbuf =
        let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { pos with
                pos_lnum = pos.pos_lnum + 1;
                pos_bol = pos.pos_cnum;
		}


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
    | '{' { let pos = lexbuf.lex_curr_p in
                    let lin = pos.pos_lnum
                    and col = pos.pos_cnum - pos.pos_bol - 1 in comentario_in_line lin col 0 lexbuf }
    | "{*" { let pos = lexbuf.lex_curr_p in
                    let lin = pos.pos_lnum
                    and col = pos.pos_cnum - pos.pos_bol - 1 in comentario_bloco lin col 0 lexbuf }
    | '+' { MAIS }
    | '-' { MENOS }
    | '*' { MULTIPLICA }
    | "div" { MODULO }
    | '/' { DIVIDE }
    | '=' { IGUAL }
    | '<' { MENOR }
    | '>' { MAIOR }
    | '(' { APAR }
    | ')' { FPAR }
    | '.' { PONTO }
    | ',' { VIRG }
    | ':' { DPONTOS }
    | ';' { PVIRG }
    | "<>" { DIFERENTE }
    | "<=" { MENORIGUAL }
    | ">=" { MAIORIGUAL }
    | "and" { E }
    | "or" { OU }
    | "not" { NOT }
    | ":=" { ATRIB }
    | "program" { PROGRAM }
    | "begin" { BEGIN }
    | "end" { END }
    | "write" { ESCREVA }
    | "writeln" { ESCREVALN }
    | "read" { LEIA }
    | "readln" { LEIALN }
    | "var" { VAR }
    | "string" { STRING }
    | "integer" { INTEGER }
    | "real" { REAL }
    | "boolean" { BOOLEAN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "for" { FOR }
    | "to" { TO }
    | "do" { DO }
    | "while" { WHILE }
    | "case" { CASE }
    | "of" { OF }
    | "function" { FUNCTION }
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
    | _ as c { raise (Erro ("Caracter desconhecido: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }

    and leia_string lin col buffer = parse
          '''  { Buffer.contents buffer}
        | "\\t"  { Buffer.add_char buffer '\t' ;
                    leia_string lin col buffer lexbuf }
        | "\\n"  { Buffer.add_char buffer '\n' ;
                    leia_string lin col buffer lexbuf }
        | '\\' ''' { Buffer.add_char buffer '\'' ;
                    leia_string lin col buffer lexbuf }
        | '\\' '\\' { Buffer.add_char buffer '\\' ;
                    leia_string lin col buffer lexbuf }
        | _ as c { Buffer.add_char buffer c;
                    leia_string lin col buffer lexbuf }
        | eof  { raise (Erro "A string não foi fechada") }

    and comentario_bloco lin col n = parse
        "*}" { if n=0 then token lexbuf
                else comentario_bloco lin col (n-1) lexbuf }
        | "{*"  { comentario_bloco lin col (n+1) lexbuf }
        | novalinha { incr_num_linha lexbuf;
                    comentario_bloco lin col n lexbuf }
        | _  { comentario_bloco lin col n lexbuf }
        | eof { raise (Erro "Comentário de bloco não fechado") }

    and comentario_in_line lin col n = parse
        '}' { if n=0 then token lexbuf
                else comentario_in_line lin col (n-1) lexbuf }
        | '{'  { comentario_in_line lin col (n+1) lexbuf }
        | novalinha { raise (Erro "Comentário de linha não fechado") }
        | _  { comentario_in_line lin col n lexbuf }
        | eof { raise (Erro "Comentário de linha não fechado") }
