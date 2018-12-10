%{
open Lexing
open Ast
open Sast
%}

%token <int * Lexing.position> LITINT
%token <float * Lexing.position> LITREAL
%token <string * Lexing.position> LITSTRING
%token <bool * Lexing.position> LITBOOL
%token <string * Lexing.position> ID
%token <Lexing.position> MAIS MENOS MULTIPLICA DIVIDE IGUAL
%token <Lexing.position> MODULO
%token <Lexing.position> MENOR MAIOR
%token <Lexing.position> DIFERENTE
%token <Lexing.position> APAR FPAR
%token <Lexing.position> PONTO VIRG DPONTOS PVIRG
%token <Lexing.position> MENORIGUAL MAIORIGUAL
%token <Lexing.position> E OU
%token <Lexing.position> ATRIB
%token <Lexing.position> PROGRAM
%token <Lexing.position> BEGIN
%token <Lexing.position> END
%token <Lexing.position> ESCREVA
%token <Lexing.position> ESCREVALN
%token <Lexing.position> LEIA
%token <Lexing.position> LEIALN
%token <Lexing.position> VAR
%token <Lexing.position> RETURN
%token <Lexing.position> STRING INTEGER REAL BOOLEAN
%token <Lexing.position> IF THEN ELSE
%token <Lexing.position> FOR TO DO WHILE CASE OF
%token <Lexing.position> FUNCTION
%token EOF

%left OU
%left E
%left IGUAL DIFERENTE MAIOR MENOR MAIORIGUAL MENORIGUAL
%left MAIS MENOS
%left MULTIPLICA DIVIDE MODULO

%start <Sast.expression Ast.programa> programa

%%

programa: PROGRAM id=ID PVIRG 
            VAR
            ds = variable_declaration*
            fs = function_declaration*
            BEGIN
                cs = statement*
            END PONTO
            EOF { Program (id, List.flatten ds, fs, cs) }


variable_declaration: ids = separated_nonempty_list(VIRG, ID) DPONTOS t = tipo PVIRG{ List.map (fun id -> DecVar (id,t)) ids }


tipo: | t = simple_type { t }

simple_type: | INTEGER { TypeInteger }
             | REAL { TypeReal }
             | BOOLEAN { TypeBoolean }
             | STRING { TypeString }

parameters: nome = ID DPONTOS t = tipo { (nome, t) }

function_declaration: FUNCTION id=ID APAR formais = separated_list(PVIRG, parameters) FPAR DPONTOS tp=simple_type PVIRG
                            VAR
                            ds = variable_declaration*
                            BEGIN
                                cs=statement*
                            END PVIRG { Function {
                                fn_nome = id;
                                fn_tiporet = tp;
                                fn_formais = formais;
                                fn_locais = List.flatten ds;
                                fn_corpo = cs 
                            }}

statement: | st = assignment_statement { st }
           | st = read_statement { st }
           | st = write_statement { st }
           | st = function_statement { st }
           | st = if_statement { st }
           | st = while_statement { st }
           | st = for_statement { st }
           | st = case_statement { st }   
           | st = return_statement { st }

assignment_statement: v=expression ATRIB e=expression PVIRG { CmdAtrib(v, e)}

function_statement: exp=chamada PVIRG {CmdFunctionCall exp }

chamada: id = ID APAR p=separated_list(VIRG, expression) FPAR {ExpChamadaF (id, p)}


if_statement: IF test=expression THEN 
              BEGIN
                st=statement*
              END
                el=option(ELSE BEGIN st2=statement* END { st2 }) 
              PVIRG  { CmdIf (test, st, el) }

read_statement: LEIA APAR xs=separated_nonempty_list(VIRG, expression) FPAR PVIRG {CmdRead xs}
                |LEIALN APAR xs=separated_nonempty_list(VIRG, expression) FPAR PVIRG {CmdReadLn xs} 

write_statement: ESCREVA APAR xs=separated_nonempty_list(VIRG, expression) FPAR PVIRG {CmdWrite xs}
                |ESCREVALN APAR xs=separated_nonempty_list(VIRG, expression) FPAR PVIRG {CmdWriteLn xs} 

for_statement: FOR v=expression ATRIB ex=expression TO e=expression DO 
               BEGIN
                    st=statement*
               END PVIRG { CmdFor(v,ex,e,st) }

while_statement: WHILE test=expression DO 
                 BEGIN
                    st=statement*
                 END PVIRG { CmdWhile (test, st)}

return_statement: RETURN e=expression? PVIRG { CmdRetorno e}

case_statement: CASE v=expression OF c = cases+ default=option(ELSE BEGIN st=statement END PVIRG {st}) END PVIRG {CmdCase(v,c,default)}

cases: e=expression DPONTOS st=statement { Case(e, st) }

expression: | v=variable { ExpVar v }
            | i=LITINT { ExpInt i }
            | s=LITSTRING { ExpString s }
            | b=LITBOOL { ExpBool b }
            | r=LITREAL { ExpReal r }
            | e1=expression op=oper e2=expression { ExpOp (op, e1, e2)}
            | APAR e=expression FPAR {e}
            | c = chamada {c}

%inline oper:
    | pos = MAIS { (Mais, pos) }
    | pos = MENOS { (Menos, pos) }
    | pos = MULTIPLICA { (Mult, pos) }
    | pos = DIVIDE { (Div, pos) }
    | pos = MODULO { (Mod, pos) }
    | pos = MENOR { (Menor, pos) }
    | pos = IGUAL { (Igual, pos) }
    | pos = MENORIGUAL { (MenorIgual, pos) }
    | pos = MAIORIGUAL { (MaiorIgual, pos) }
    | pos = MAIOR { (Maior, pos) }
    | pos = DIFERENTE { (Difer, pos) }
    | pos = E { (And, pos) }
    | pos = OU { (Or, pos) }


variable: | x=ID { VarSimples x }
          | v=variable PONTO x=ID { VarCampo (v, x) }