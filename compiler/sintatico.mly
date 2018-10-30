%{
open Ast
%}

%token <int> LITINT
%token <float> LITREAL
%token <string> LITSTRING
%token <bool> LITBOOL
%token <string> ID
%token MAIS MENOS MULTIPLICA DIVIDE IGUAL
%token MODULO
%token MENOR MAIOR
%token DIFERENTE
%token APAR FPAR
%token PONTO VIRG DPONTOS PVIRG
%token MENORIGUAL MAIORIGUAL
%token E OU NOT
%token ATRIB
%token PROGRAM
%token BEGIN
%token END
%token ESCREVA
%token ESCREVALN
%token LEIA
%token LEIALN
%token VAR
%token STRING INTEGER REAL BOOLEAN
%token IF THEN ELSE
%token FOR TO DO WHILE CASE OF
%token FUNCTION
%token EOF

%left OU
%left E
%left NOT
%left IGUAL DIFERENTE MAIOR MENOR MAIORIGUAL MENORIGUAL
%left MAIS MENOS
%left MULTIPLICA DIVIDE MODULO

%start <Ast.programa> programa

%%

programa: PROGRAM id=ID PVIRG bk=block PONTO EOF { Program (id, bk) }

block: vdp = variable_declaration_part?
       pdp = function_declaration_part* 
       sp = statement_part { Block(vdp, pdp, sp) }

variable_declaration_part: VAR
                            v=nonempty_list(vd=variable_declaration PVIRG { vd })
                            { VarDeclarationPart v }

variable_declaration: ids = separated_nonempty_list(VIRG, ID) DPONTOS t = tipo { List.map (fun id -> DecVar (id,t)) ids }

tipo: | t = simple_type { t }

simple_type: | INTEGER { TypeInteger }
             | REAL { TypeReal }
             | BOOLEAN { TypeBoolean }
             | STRING { TypeString }

parameters: ids = separated_nonempty_list(VIRG, ID) DPONTOS t=simple_type { List.map (fun id -> Parameters (id,t)) ids }

function_declaration_part: fd=function_declaration PVIRG { fd }

function_declaration: FUNCTION id=ID APAR p=parameters FPAR DPONTOS tp=simple_type PVIRG bk=block { Function (id, p, tp, bk )}

statement_part: BEGIN stb =  statement_block?  END { StatementPart stb }


statement_block: sts = nonempty_list(st = statement PVIRG {st}) { sts }

statement: | st = assignment_statement { st }
           | st = read_statement { st }
           | st = write_statement { st }
           | st = function_statement { st }
           | st = if_statement { st }
           | st = while_statement { st }
           | st = for_statement { st }
           | st = case_statement { st }

assignment_statement: v=variable ATRIB e=expression { CmdAtrib(v, e)}

read_statement: LEIA APAR xs=separated_nonempty_list(VIRG, expression) FPAR {CmdRead xs}
                |LEIALN APAR xs=separated_nonempty_list(VIRG, expression) FPAR {CmdReadLn xs} 

write_statement: ESCREVA APAR xs=separated_nonempty_list(VIRG, expression) FPAR {CmdWrite xs}
                |ESCREVALN APAR xs=separated_nonempty_list(VIRG, expression) FPAR {CmdWriteLn xs} 

function_statement: id = ID APAR p=option(arg=separated_nonempty_list(VIRG, expression) {arg}) FPAR {CmdFunctionCall (id, p)}


if_statement: IF test=expression THEN st=statement_part el=option(ELSE st2=statement_part { st2 })  { CmdIf (test, st, el) }

while_statement: WHILE test=expression DO st=statement_part { CmdWhile (test, st)}

for_statement: FOR v=variable ATRIB ex=expression TO e=expression DO st=statement_part { CmdFor(v,ex,e,st) }

case_statement: CASE v=variable OF c = cases+ default=option(ELSE st=statement_part PVIRG {st}) END {CmdCase(v,c,default)}

cases: e=expression DPONTOS st=statement PVIRG { Case(e, st) }

expression: | v=variable { ExpVar v }
            | i=LITINT { ExpInt i }
            | r=LITREAL { ExpReal r }
            | s=LITSTRING { ExpString s }
            | b=LITBOOL { ExpBool b }
            | MENOS e=expression { ExpMen e }
            | NOT e=expression { ExpNot e }
            | e1=expression op=oper e2=expression { ExpOp (op, e1, e2)}
            | APAR e=expression FPAR { Expar (e) }
            | c = function_statement {ExpChamadaF c}

%inline oper:
    | MAIS { Mais }
    | MENOS { Menos }
    | MULTIPLICA { Mult }
    | DIVIDE { Div }
    | MODULO { Mod }
    | MENOR { Menor }
    | IGUAL { Igual }
    | MENORIGUAL { MenorIgual }
    | MAIORIGUAL { MaiorIgual }
    | MAIOR { Maior }
    | DIFERENTE { Difer }
    | E { And }
    | OU { Or }


variable: | x=ID { VarSimples x }
          | v=variable PONTO x=ID { VarCampo (v, x) }