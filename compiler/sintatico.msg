programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS ID APAR FPAR PVIRG ELSE BEGIN END PVIRG WHILE 
##
## Ends in an error in state: 128.
##
## case_statement -> CASE variable OF nonempty_list(cases) option(__anonymous_4) . END [ PVIRG ]
##
## The known suffix of the stack is as follows:
## CASE variable OF nonempty_list(cases) option(__anonymous_4) 
##

"end" esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS ID APAR FPAR PVIRG ELSE BEGIN END WHILE 
##
## Ends in an error in state: 126.
##
## option(__anonymous_4) -> ELSE statement_part . PVIRG [ END ]
##
## The known suffix of the stack is as follows:
## ELSE statement_part 
##

";" esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS ID APAR FPAR PVIRG ELSE WHILE 
##
## Ends in an error in state: 125.
##
## option(__anonymous_4) -> ELSE . statement_part PVIRG [ END ]
##
## The known suffix of the stack is as follows:
## ELSE 
##

"begin" esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS ID APAR FPAR PVIRG WHILE 
##
## Ends in an error in state: 145.
##
## nonempty_list(cases) -> cases . [ END ELSE ]
## nonempty_list(cases) -> cases . nonempty_list(cases) [ END ELSE ]
##
## The known suffix of the stack is as follows:
## cases 
##

"end" esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS ID APAR FPAR WHILE 
##
## Ends in an error in state: 137.
##
## cases -> expression DPONTOS statement . PVIRG [ NOT MENOS LITSTRING LITREAL LITINT LITBOOL ID END ELSE APAR ]
##
## The known suffix of the stack is as follows:
## expression DPONTOS statement 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING DPONTOS VIRG 
##
## Ends in an error in state: 131.
##
## cases -> expression DPONTOS . statement PVIRG [ NOT MENOS LITSTRING LITREAL LITINT LITBOOL ID END ELSE APAR ]
##
## The known suffix of the stack is as follows:
## expression DPONTOS 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN CASE ID OF LITSTRING WHILE 
##
## Ends in an error in state: 130.
##
## cases -> expression . DPONTOS statement PVIRG [ NOT MENOS LITSTRING LITREAL LITINT LITBOOL ID END ELSE APAR ]
## expression -> expression . MAIS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . E expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DPONTOS DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression 
##

":" esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID OF WHILE 
##
## Ends in an error in state: 123.
##
## case_statement -> CASE variable OF . nonempty_list(cases) option(__anonymous_4) END [ PVIRG ]
##
## The known suffix of the stack is as follows:
## CASE variable OF 
##

<caso> esperado

programa: PROGRAM ID PVIRG BEGIN CASE ID WHILE 
##
## Ends in an error in state: 122.
##
## case_statement -> CASE variable . OF nonempty_list(cases) option(__anonymous_4) END [ PVIRG ]
## variable -> variable . PONTO ID [ PONTO OF ]
##
## The known suffix of the stack is as follows:
## CASE variable 
##

"of" esperado

programa: PROGRAM ID PVIRG BEGIN CASE WHILE 
##
## Ends in an error in state: 121.
##
## case_statement -> CASE . variable OF nonempty_list(cases) option(__anonymous_4) END [ PVIRG ]
##
## The known suffix of the stack is as follows:
## CASE 
##

<variavel> esperada

programa: PROGRAM ID PVIRG BEGIN END PONTO WHILE 
##
## Ends in an error in state: 160.
##
## programa -> PROGRAM ID PVIRG block PONTO . EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAM ID PVIRG block PONTO 
##

"eof" esperado

programa: PROGRAM ID PVIRG BEGIN END WHILE 
##
## Ends in an error in state: 159.
##
## programa -> PROGRAM ID PVIRG block . PONTO EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAM ID PVIRG block 
##

"." esperado

programa: PROGRAM ID PVIRG BEGIN ESCREVA APAR WHILE 
##
## Ends in an error in state: 118.
##
## write_statement -> ESCREVA APAR . separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## ESCREVA APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN ESCREVA WHILE 
##
## Ends in an error in state: 117.
##
## write_statement -> ESCREVA . APAR separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## ESCREVA 
##

"(" esperado

programa: PROGRAM ID PVIRG BEGIN ESCREVALN APAR WHILE 
##
## Ends in an error in state: 114.
##
## write_statement -> ESCREVALN APAR . separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## ESCREVALN APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN ESCREVALN WHILE 
##
## Ends in an error in state: 113.
##
## write_statement -> ESCREVALN . APAR separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## ESCREVALN 
##

"(" esperado

programa: PROGRAM ID PVIRG BEGIN FOR ID ATRIB LITSTRING TO LITSTRING DO WHILE 
##
## Ends in an error in state: 111.
##
## for_statement -> FOR variable ATRIB expression TO expression DO . statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR variable ATRIB expression TO expression DO 
##

"begin" esperado

programa: PROGRAM ID PVIRG BEGIN FOR ID ATRIB LITSTRING TO LITSTRING WHILE 
##
## Ends in an error in state: 110.
##
## expression -> expression . MAIS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## for_statement -> FOR variable ATRIB expression TO expression . DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR variable ATRIB expression TO expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN FOR ID ATRIB LITSTRING TO WHILE 
##
## Ends in an error in state: 109.
##
## for_statement -> FOR variable ATRIB expression TO . expression DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR variable ATRIB expression TO 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN FOR ID ATRIB LITSTRING WHILE 
##
## Ends in an error in state: 108.
##
## expression -> expression . MAIS expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . E expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ TO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## for_statement -> FOR variable ATRIB expression . TO expression DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR variable ATRIB expression 
##

"to" esperado

programa: PROGRAM ID PVIRG BEGIN FOR ID ATRIB WHILE 
##
## Ends in an error in state: 107.
##
## for_statement -> FOR variable ATRIB . expression TO expression DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR variable ATRIB 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN FOR ID WHILE 
##
## Ends in an error in state: 106.
##
## for_statement -> FOR variable . ATRIB expression TO expression DO statement_part [ PVIRG ]
## variable -> variable . PONTO ID [ PONTO ATRIB ]
##
## The known suffix of the stack is as follows:
## FOR variable 
##

":=" esperado

programa: PROGRAM ID PVIRG BEGIN FOR WHILE 
##
## Ends in an error in state: 104.
##
## for_statement -> FOR . variable ATRIB expression TO expression DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FOR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN ID APAR FPAR PVIRG VIRG 
##
## Ends in an error in state: 149.
##
## nonempty_list(__anonymous_1) -> statement PVIRG . [ END ]
## nonempty_list(__anonymous_1) -> statement PVIRG . nonempty_list(__anonymous_1) [ END ]
##
## The known suffix of the stack is as follows:
## statement PVIRG 
##

"end" esperado;

programa: PROGRAM ID PVIRG BEGIN ID APAR FPAR WHILE 
##
## Ends in an error in state: 148.
##
## nonempty_list(__anonymous_1) -> statement . PVIRG [ END ]
## nonempty_list(__anonymous_1) -> statement . PVIRG nonempty_list(__anonymous_1) [ END ]
##
## The known suffix of the stack is as follows:
## statement 
##

";" esperado

programa: PROGRAM ID PVIRG BEGIN ID APAR WHILE 
##
## Ends in an error in state: 44.
##
## function_statement -> ID APAR . option(__anonymous_2) FPAR [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## ID APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN ID ATRIB LITSTRING WHILE 
##
## Ends in an error in state: 136.
##
## assignment_statement -> variable ATRIB expression . [ PVIRG ]
## expression -> expression . MAIS expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . E expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## variable ATRIB expression 
##

";" esperado

programa: PROGRAM ID PVIRG BEGIN ID ATRIB WHILE 
##
## Ends in an error in state: 135.
##
## assignment_statement -> variable ATRIB . expression [ PVIRG ]
##
## The known suffix of the stack is as follows:
## variable ATRIB 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN ID PONTO WHILE 
##
## Ends in an error in state: 47.
##
## variable -> variable PONTO . ID [ VIRG TO THEN PVIRG PONTO OU OF MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ATRIB ]
##
## The known suffix of the stack is as follows:
## variable PONTO 
##

<id> esperado

programa: PROGRAM ID PVIRG BEGIN ID VIRG 
##
## Ends in an error in state: 134.
##
## assignment_statement -> variable . ATRIB expression [ PVIRG ]
## variable -> variable . PONTO ID [ PONTO ATRIB ]
##
## The known suffix of the stack is as follows:
## variable 
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 43, spurious reduction of production variable -> ID 
##

<id> esperado

programa: PROGRAM ID PVIRG BEGIN ID WHILE 
##
## Ends in an error in state: 43.
##
## function_statement -> ID . APAR option(__anonymous_2) FPAR [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## variable -> ID . [ VIRG TO THEN PVIRG PONTO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ATRIB ]
##
## The known suffix of the stack is as follows:
## ID 
##

":=" esperado

programa: PROGRAM ID PVIRG BEGIN IF LITSTRING THEN BEGIN END ELSE WHILE 
##
## Ends in an error in state: 101.
##
## option(__anonymous_3) -> ELSE . statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## ELSE 
##

"begin" esperado

programa: PROGRAM ID PVIRG BEGIN IF LITSTRING THEN BEGIN END WHILE 
##
## Ends in an error in state: 100.
##
## if_statement -> IF expression THEN statement_part . option(__anonymous_3) [ PVIRG ]
##
## The known suffix of the stack is as follows:
## IF expression THEN statement_part 
##

";" esperado

programa: PROGRAM ID PVIRG BEGIN IF LITSTRING THEN WHILE 
##
## Ends in an error in state: 99.
##
## if_statement -> IF expression THEN . statement_part option(__anonymous_3) [ PVIRG ]
##
## The known suffix of the stack is as follows:
## IF expression THEN 
##

"begin" esperado

programa: PROGRAM ID PVIRG BEGIN IF LITSTRING WHILE 
##
## Ends in an error in state: 98.
##
## expression -> expression . MAIS expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . E expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ THEN OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DIVIDE DIFERENTE ]
## if_statement -> IF expression . THEN statement_part option(__anonymous_3) [ PVIRG ]
##
## The known suffix of the stack is as follows:
## IF expression 
##

"then" esperado

programa: PROGRAM ID PVIRG BEGIN IF WHILE 
##
## Ends in an error in state: 97.
##
## if_statement -> IF . expression THEN statement_part option(__anonymous_3) [ PVIRG ]
##
## The known suffix of the stack is as follows:
## IF 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN LEIA APAR WHILE 
##
## Ends in an error in state: 94.
##
## read_statement -> LEIA APAR . separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## LEIA APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN LEIA WHILE 
##
## Ends in an error in state: 93.
##
## read_statement -> LEIA . APAR separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## LEIA 
##

"(" esperado

programa: PROGRAM ID PVIRG BEGIN LEIALN APAR LITSTRING VIRG WHILE 
##
## Ends in an error in state: 82.
##
## separated_nonempty_list(VIRG,expression) -> expression VIRG . separated_nonempty_list(VIRG,expression) [ FPAR ]
##
## The known suffix of the stack is as follows:
## expression VIRG 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN LEIALN APAR LITSTRING WHILE 
##
## Ends in an error in state: 81.
##
## expression -> expression . MAIS expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## separated_nonempty_list(VIRG,expression) -> expression . [ FPAR ]
## separated_nonempty_list(VIRG,expression) -> expression . VIRG separated_nonempty_list(VIRG,expression) [ FPAR ]
##
## The known suffix of the stack is as follows:
## expression 
##

")" esperado

programa: PROGRAM ID PVIRG BEGIN LEIALN APAR WHILE 
##
## Ends in an error in state: 90.
##
## read_statement -> LEIALN APAR . separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## LEIALN APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN LEIALN WHILE 
##
## Ends in an error in state: 89.
##
## read_statement -> LEIALN . APAR separated_nonempty_list(VIRG,expression) FPAR [ PVIRG ]
##
## The known suffix of the stack is as follows:
## LEIALN 
##

"(" esperado

programa: PROGRAM ID PVIRG BEGIN VIRG 
##
## Ends in an error in state: 35.
##
## statement_part -> BEGIN . option(statement_block) END [ PVIRG PONTO ELSE ]
##
## The known suffix of the stack is as follows:
## BEGIN 
##

"end" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE APAR LITSTRING WHILE 
##
## Ends in an error in state: 50.
##
## expression -> expression . MAIS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . E expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DIVIDE DIFERENTE ]
## expression -> APAR expression . FPAR [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## APAR expression 
##

")" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE APAR WHILE 
##
## Ends in an error in state: 45.
##
## expression -> APAR . expression FPAR [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## APAR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE ID ATRIB 
##
## Ends in an error in state: 46.
##
## expression -> variable . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## variable -> variable . PONTO ID [ VIRG TO THEN PVIRG PONTO OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## variable 
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 43, spurious reduction of production variable -> ID 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING DIFERENTE LITSTRING WHILE 
##
## Ends in an error in state: 76.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression DIFERENTE expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression DIFERENTE expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING DIFERENTE WHILE 
##
## Ends in an error in state: 75.
##
## expression -> expression DIFERENTE . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression DIFERENTE 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING DIVIDE WHILE 
##
## Ends in an error in state: 59.
##
## expression -> expression DIVIDE . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression DIVIDE 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING DO WHILE 
##
## Ends in an error in state: 87.
##
## while_statement -> WHILE expression DO . statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## WHILE expression DO 
##

"begin" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING E LITSTRING WHILE 
##
## Ends in an error in state: 74.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression E expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression E expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING E WHILE 
##
## Ends in an error in state: 73.
##
## expression -> expression E . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression E 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING IGUAL LITSTRING WHILE 
##
## Ends in an error in state: 72.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression IGUAL expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression IGUAL expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING IGUAL WHILE 
##
## Ends in an error in state: 71.
##
## expression -> expression IGUAL . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression IGUAL 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIOR LITSTRING WHILE 
##
## Ends in an error in state: 70.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MAIOR expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIOR expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIOR WHILE 
##
## Ends in an error in state: 69.
##
## expression -> expression MAIOR . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIOR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIORIGUAL LITSTRING WHILE 
##
## Ends in an error in state: 68.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MAIORIGUAL expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIORIGUAL expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIORIGUAL WHILE 
##
## Ends in an error in state: 67.
##
## expression -> expression MAIORIGUAL . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIORIGUAL 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIS LITSTRING WHILE 
##
## Ends in an error in state: 64.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MAIS expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIS expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MAIS WHILE 
##
## Ends in an error in state: 63.
##
## expression -> expression MAIS . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MAIS 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENOR LITSTRING WHILE 
##
## Ends in an error in state: 66.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MENOR expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENOR expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENOR WHILE 
##
## Ends in an error in state: 65.
##
## expression -> expression MENOR . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENOR 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENORIGUAL LITSTRING WHILE 
##
## Ends in an error in state: 62.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MENORIGUAL expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENORIGUAL expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENORIGUAL WHILE 
##
## Ends in an error in state: 61.
##
## expression -> expression MENORIGUAL . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENORIGUAL 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENOS LITSTRING WHILE 
##
## Ends in an error in state: 58.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression MENOS expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENOS expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MENOS WHILE 
##
## Ends in an error in state: 57.
##
## expression -> expression MENOS . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MENOS 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MODULO WHILE 
##
## Ends in an error in state: 55.
##
## expression -> expression MODULO . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MODULO 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING MULTIPLICA WHILE 
##
## Ends in an error in state: 53.
##
## expression -> expression MULTIPLICA . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression MULTIPLICA 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING OU LITSTRING WHILE 
##
## Ends in an error in state: 52.
##
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression OU expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression OU expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING OU WHILE 
##
## Ends in an error in state: 51.
##
## expression -> expression OU . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## expression OU 
##

<expresssão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE LITSTRING WHILE 
##
## Ends in an error in state: 86.
##
## expression -> expression . MAIS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL E DO DIVIDE DIFERENTE ]
## while_statement -> WHILE expression . DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## WHILE expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE MENOS LITSTRING WHILE 
##
## Ends in an error in state: 84.
##
## expression -> MENOS expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## MENOS expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE MENOS WHILE 
##
## Ends in an error in state: 38.
##
## expression -> MENOS . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## MENOS 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE NOT LITSTRING WHILE 
##
## Ends in an error in state: 85.
##
## expression -> NOT expression . [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOS expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MULTIPLICA expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIVIDE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MODULO expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . IGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MENORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIORIGUAL expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . MAIOR expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . DIFERENTE expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . E expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
## expression -> expression . OU expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## NOT expression 
##

"do" esperado

programa: PROGRAM ID PVIRG BEGIN WHILE NOT WHILE 
##
## Ends in an error in state: 37.
##
## expression -> NOT . expression [ VIRG TO THEN PVIRG OU MULTIPLICA MODULO MENOS MENORIGUAL MENOR MAIS MAIORIGUAL MAIOR IGUAL FPAR E DPONTOS DO DIVIDE DIFERENTE ]
##
## The known suffix of the stack is as follows:
## NOT 
##

<expressão> esperada

programa: PROGRAM ID PVIRG BEGIN WHILE WHILE 
##
## Ends in an error in state: 36.
##
## while_statement -> WHILE . expression DO statement_part [ PVIRG ]
##
## The known suffix of the stack is as follows:
## WHILE 
##

"end" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR DPONTOS BOOLEAN PVIRG BEGIN END PVIRG WHILE 
##
## Ends in an error in state: 155.
##
## list(function_declaration_part) -> function_declaration_part . list(function_declaration_part) [ BEGIN ]
##
## The known suffix of the stack is as follows:
## function_declaration_part 
##

"begin" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR DPONTOS BOOLEAN PVIRG BEGIN END WHILE 
##
## Ends in an error in state: 157.
##
## function_declaration_part -> function_declaration . PVIRG [ FUNCTION BEGIN ]
##
## The known suffix of the stack is as follows:
## function_declaration 
##

";" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR DPONTOS STRING PVIRG WHILE 
##
## Ends in an error in state: 32.
##
## function_declaration -> FUNCTION ID APAR parameters FPAR DPONTOS simple_type PVIRG . block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR parameters FPAR DPONTOS simple_type PVIRG 
##

"begin" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR DPONTOS STRING WHILE 
##
## Ends in an error in state: 31.
##
## function_declaration -> FUNCTION ID APAR parameters FPAR DPONTOS simple_type . PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR parameters FPAR DPONTOS simple_type 
##

";" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR DPONTOS WHILE 
##
## Ends in an error in state: 30.
##
## function_declaration -> FUNCTION ID APAR parameters FPAR DPONTOS . simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR parameters FPAR DPONTOS 
##

<tipo> esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN FPAR WHILE 
##
## Ends in an error in state: 29.
##
## function_declaration -> FUNCTION ID APAR parameters FPAR . DPONTOS simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR parameters FPAR 
##

<: tipo> esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS BOOLEAN WHILE 
##
## Ends in an error in state: 28.
##
## function_declaration -> FUNCTION ID APAR parameters . FPAR DPONTOS simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR parameters 
##

")" esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR ID DPONTOS WHILE 
##
## Ends in an error in state: 26.
##
## parameters -> separated_nonempty_list(VIRG,ID) DPONTOS . simple_type [ FPAR ]
##
## The known suffix of the stack is as follows:
## separated_nonempty_list(VIRG,ID) DPONTOS 
##

<tipo> esperado

programa: PROGRAM ID PVIRG FUNCTION ID APAR WHILE 
##
## Ends in an error in state: 24.
##
## function_declaration -> FUNCTION ID APAR . parameters FPAR DPONTOS simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID APAR 
##

")" esperado

programa: PROGRAM ID PVIRG FUNCTION ID WHILE 
##
## Ends in an error in state: 23.
##
## function_declaration -> FUNCTION ID . APAR parameters FPAR DPONTOS simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION ID 
##

"(" esperado

programa: PROGRAM ID PVIRG FUNCTION WHILE 
##
## Ends in an error in state: 22.
##
## function_declaration -> FUNCTION . ID APAR parameters FPAR DPONTOS simple_type PVIRG block [ PVIRG ]
##
## The known suffix of the stack is as follows:
## FUNCTION 
##

<id> esperado

programa: PROGRAM ID PVIRG VAR ID DPONTOS BOOLEAN PVIRG WHILE 
##
## Ends in an error in state: 9.
##
## nonempty_list(__anonymous_0) -> variable_declaration PVIRG . [ FUNCTION BEGIN ]
## nonempty_list(__anonymous_0) -> variable_declaration PVIRG . nonempty_list(__anonymous_0) [ FUNCTION BEGIN ]
##
## The known suffix of the stack is as follows:
## variable_declaration PVIRG 
##

"begin" esperado

programa: PROGRAM ID PVIRG VAR ID DPONTOS BOOLEAN WHILE 
##
## Ends in an error in state: 8.
##
## nonempty_list(__anonymous_0) -> variable_declaration . PVIRG [ FUNCTION BEGIN ]
## nonempty_list(__anonymous_0) -> variable_declaration . PVIRG nonempty_list(__anonymous_0) [ FUNCTION BEGIN ]
##
## The known suffix of the stack is as follows:
## variable_declaration 
##

";" esperado

programa: PROGRAM ID PVIRG VAR ID DPONTOS WHILE 
##
## Ends in an error in state: 11.
##
## variable_declaration -> separated_nonempty_list(VIRG,ID) DPONTOS . tipo [ PVIRG ]
##
## The known suffix of the stack is as follows:
## separated_nonempty_list(VIRG,ID) DPONTOS 
##

<tipo> esperado

programa: PROGRAM ID PVIRG VAR ID VIRG WHILE 
##
## Ends in an error in state: 6.
##
## separated_nonempty_list(VIRG,ID) -> ID VIRG . separated_nonempty_list(VIRG,ID) [ DPONTOS ]
##
## The known suffix of the stack is as follows:
## ID VIRG 
##

<id [, ...id] : tipo> esperado

programa: PROGRAM ID PVIRG VAR ID WHILE 
##
## Ends in an error in state: 5.
##
## separated_nonempty_list(VIRG,ID) -> ID . [ DPONTOS ]
## separated_nonempty_list(VIRG,ID) -> ID . VIRG separated_nonempty_list(VIRG,ID) [ DPONTOS ]
##
## The known suffix of the stack is as follows:
## ID 
##

<[, ...id'] : tipo> esperado

programa: PROGRAM ID PVIRG VAR WHILE 
##
## Ends in an error in state: 4.
##
## variable_declaration_part -> VAR . nonempty_list(__anonymous_0) [ FUNCTION BEGIN ]
##
## The known suffix of the stack is as follows:
## VAR 
##

<id [, ...id'] : tipo> esperado

programa: PROGRAM ID PVIRG WHILE 
##
## Ends in an error in state: 3.
##
## programa -> PROGRAM ID PVIRG . block PONTO EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAM ID PVIRG 
##

"begin" esperado

programa: PROGRAM ID WHILE 
##
## Ends in an error in state: 2.
##
## programa -> PROGRAM ID . PVIRG block PONTO EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAM ID 
##

";" esperado

programa: PROGRAM WHILE 
##
## Ends in an error in state: 1.
##
## programa -> PROGRAM . ID PVIRG block PONTO EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAM 
##

<id> esperado

programa: WHILE 
##
## Ends in an error in state: 0.
##
## programa' -> . programa [ # ]
##
## The known suffix of the stack is as follows:
## 
##

"program" esperado

