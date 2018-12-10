open Printf
open Lexing

open Ast
exception Erro_Sintatico of string

module S = MenhirLib.General (* Streams *)
module I = Sintatico.MenhirInterpreter

open Semantico
open Codigo
open Cod3End

let message =
  fun s ->
    match s with
    | 0 ->
        "\"program\" esperado\n"
    | 1 ->
        "<id> esperado\n"
    | 2 ->
        "\";\" esperado\n"
    | 3 ->
        "\"var\" esperado\n"
    | 4 ->
        "\"begin\" esperado\n"
    | 5 ->
        "<:tipo_variavel> esperado\n"
    | 6 ->
        "<id> esperado\n"
    | 10 ->
        "<tipo_variavel> esperado\n"
    | 15 ->
        "\";\" esperado\n"
    | 8 ->
        "\"begin\" esperado\n"
    | 20 ->
        "<id> esperado\n"
    | 21 ->
        "\"(\" esperado\n"
    | 22 ->
        "\")\" esperado\n"
    | 23 ->
        "<:tipo_parametro> esperado\n"
    | 24 ->
        "<tipo_parametro> esperado\n"
    | 27 ->
        "\")\" esperado\n"
    | 28 ->
        "<id> esperado\n"
    | 31 ->
        "<:tipo_retorno> esperado\n"
    | 32 ->
        "<tipo_retorno> esperado\n"
    | 33 ->
        "\";\" esperado\n"
    | 34 ->
        "\"var\" esperado\n"
    | 35 ->
        "\"begin\" esperado\n"
    | 36 ->
        "\"begin\" esperado\n"
    | 37 ->
        "\"end\" esperado\n"
    | 172 ->
        "\";\" esperado\n"
    | 180 ->
        "\"begin\" esperado\n"
    | 38 ->
        "<expressao> esperado\n"
    | 84 ->
        "\"do\" esperado\n"
    | 85 ->
        "\"begin\" esperado\n"
    | 86 ->
        "\"end\" esperado\n"
    | 169 ->
        "\";\"\" esperado\n"
    | 175 ->
        "\"end\" esperado\n"
    | 87 ->
        "<expressao> esperado\n"
    | 155 ->
        "\"end\" esperado\n"
    | 90 ->
        "\";\" esperado\n"
    | 139 ->
        "\":=\" esperado\n"
    | 50 ->
        "<expressao> esperado\n"
    | 51 ->
        "\";\" esperado\n"
    | 52 ->
        "<expressao> esperado\n"
    | 55 ->
        "<expressao> esperado\n"
    | 57 ->
        "<expressao> esperado\n"
    | 58 ->
        "\";\" esperado\n"
    | 61 ->
        "<expressao> esperado\n"
    | 62 ->
        "\";\" esperado\n"
    | 65 ->
        "<expressao> esperado\n"
    | 66 ->
        "\";\" esperado\n"
    | 63 ->
        "<expressao> esperado\n"
    | 64 ->
        "\";\" esperado\n"
    | 67 ->
        "<expressao> esperado\n"
    | 68 ->
        "\";\" esperado\n"
    | 69 ->
        "<expressao> esperado\n"
    | 70 ->
        "\";\" esperado\n"
    | 71 ->
        "<expressao> esperado\n"
    | 72 ->
        "\";\" esperado\n"
    | 73 ->
        "<expressao> esperado\n"
    | 74 ->
        "\";\" esperado\n"
    | 59 ->
        "<expressao> esperado\n"
    | 75 ->
        "<expressao> esperado\n"
    | 76 ->
        "\";\" esperado\n"
    | 140 ->
        "<expressao> esperado\n"
    | 141 ->
        "\";\" esperado\n"
    | 91 ->
        "\"(\" esperado\n"
    | 92 ->
        "<expressao> esperado\n"
    | 81 ->
        "\")\" esperado\n"
    | 82 ->
        "<expressao> esperado\n"
    | 94 ->
        "\";\" esperado\n"
    | 96 ->
        "\"(\" esperado\n"
    | 97 ->
        "<expressao> esperado\n"
    | 99 ->
        "\";\" esperado\n"
    | 101 ->
        "<expressao> esperado\n"
    | 102 ->
        "\"then\" esperado\n"
    | 103 ->
        "\"begin\" esperado\n"
    | 104 ->
        "\"end\" esperado\n"
    | 161 ->
        "\";\" esperado\n"
    | 162 ->
        "\"begin\" esperado\n"
    | 163 ->
        "\"end\" esperado\n"
    | 166 ->
        "\";\" esperado\n"
    | 43 ->
        "\":=\" esperado\n"
    | 47 ->
        "<campo> esperado\n"
    | 46 ->
        "\";=\" esperado\n"
    | 44 ->
        "\")\" esperado\n"
    | 143 ->
        "\";\" esperado\n"
    | 105 ->
        "<expressao> esperado\n"
    | 106 ->
        "\":=\" esperado\n"
    | 107 ->
        "<expressao> esperado\n"
    | 108 ->
        "\"to\" esperado\n"
    | 109 ->
        "<expressao> esperado\n"
    | 110 ->
        "\"do\" esperado\n"
    | 111 ->
        "\"begin\" esperado\n"
    | 112 ->
        "\"end\" esperado\n"
    | 158 ->
        "\";\" esperado\n"
    | 113 ->
        "\"(\" esperado\n"
    | 114 ->
        "\")\" esperado\n"
    | 116 ->
        "\";\" esperado\n"
    | 118 ->
        "\"(\" esperado\n"
    | 119 ->
        "\")\" esperado\n"
    | 121 ->
        "\";\" esperado\n"
    | 177 ->
        "\".\" esperado\n"
    | 178 ->
        "\"eof\" esperado\n"
    | 123 ->
        "<expressao> esperado\n"
    | 124 ->
        "\"of\" esperado\n"
    | 125 ->
        "<case> esperado\n"
    | 150 ->
        "\":\" esperado\n"
    | 151 ->
        "<expressao> esperado\n"
    | 153 ->
        "\"end\" esperado\n"
    | 148 ->
        "\";\" esperado\n"
    | 127 ->
        "<expressao> esperado\n"
    | 128 ->
        "<expressao> esperado\n"
    | 131 ->
        "\"end\" esperado\n"
    | 132 ->
        "\";\" esperado\n"
    | 147 ->
        "\"end\" esperado\n"
    | 45 ->
        "\")\" esperado\n"
    | 49 ->
        "\")\" esperado\n"
    | _ ->
        raise Not_found
let posicao lexbuf =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    sprintf "linha %d, coluna %d" lin col

(* [pilha checkpoint] extrai a pilha do autômato LR(1) contida em checkpoint *)

let pilha checkpoint =
  match checkpoint with
  | I.HandlingError amb -> I.stack amb
  | _ -> assert false (* Isso não pode acontecer *)

let estado checkpoint : int =
  match Lazy.force (pilha checkpoint) with
  | S.Nil -> (* O parser está no estado inicial *)
     0
  | S.Cons (I.Element (s, _, _, _), _) ->
     I.number s

let sucesso v = Some v

let falha lexbuf (checkpoint : (Sast.expression Ast.programa) I.checkpoint) =
  let estado_atual = estado checkpoint in
  let msg = message estado_atual in
  raise (Erro_Sintatico (Printf.sprintf "%d - %s.\n"
                                      (Lexing.lexeme_start lexbuf) msg))

let loop lexbuf resultado =
  let fornecedor = I.lexer_lexbuf_to_supplier Lexico.token lexbuf in
  I.loop_handle sucesso (falha lexbuf) fornecedor resultado


let parse_com_erro lexbuf =
  try
    Some (loop lexbuf (Sintatico.Incremental.programa lexbuf.lex_curr_p))
  with
  | Lexico.Erro msg ->
     printf "Erro lexico na %s:\n\t%s\n" (posicao lexbuf) msg;
     None
  | Erro_Sintatico msg ->
     printf "Erro sintático na %s %s\n" (posicao lexbuf) msg;
     None

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = parse_com_erro lexbuf in
  ast

let parse_arq nome =
  let ic = open_in nome in
  let lexbuf = Lexing.from_channel ic in
  let ast = parse_com_erro lexbuf in
  let _ = close_in ic in
  ast

let verifica_tipos nome =
  let ast = parse_arq nome in
  match ast with
    Some (Some ast) -> semantico ast
  | _ -> failwith "Nada a fazer!\n"

let traduz nome =
  let (arv,tab) = verifica_tipos nome in
  tradutor arv

let imprime_traducao cod =
   let _ = printf "\n" in
   escreve_codigo cod


(* Para compilar:
     ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --table" -package menhirLib cod3endTest.byte
  
   Para usar, entre no ocaml 

     rlwrap ocaml

   e se desejar ver apenas a árvore sintática que sai do analisador sintático, digite

     parse_arq "exemplos/Tipos/ex8.tip";;

   Depois, para ver a saída do analisador semântico já com a árvore anotada com 
   o tipos, digite:

   verifica_tipos "exemplos/Tipos/ex8.tip";;

   Note que o analisador semântico está retornando também o ambiente global. Se 
   quiser separá-los, digite:

   let (arv, amb) = verifica_tipos "exemplos/Tipos/ex8.tip";;
   
   Para ver o código de 3 endereços:
   traduz "exemplos/Tipos/ex8.tip";;
   
   ou se quiser ver em um formato mais legível:
   
   let cod = traduz "exemplos/Tipos/ex8.tip" in imprime_traducao cod;;
   

    

 *)
