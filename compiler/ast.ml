type id = string

type programa = Program of id * block
and block = Block of variable_declaration_part option * function_declaration_part list * statement_part

and variable_declaration_part = VarDeclarationPart of variable_declarations list
and variable_declarations = variable_declaration list
and variable_declaration = DecVar of id * tipo

and function_declaration_part = function_declaration
and function_declaration = Function of id * parameters * tipo * block

and parameters = parameter list
and parameter = Parameters of id * tipo

and tipo = | TypeInteger
           | TypeReal
           | TypeBoolean
           | TypeString

and cases = Case of expression * statement

and statement_part = StatementPart of statement_block option

and statement_block = statement list

and statement =   | CmdAtrib of variable * expression
                  | CmdRead of expressions
                  | CmdReadLn of expressions
                  | CmdWrite of expressions
                  | CmdWriteLn of expressions
                  | CmdFunctionCall of id * expressions option
                  | CmdIf of expression * statement_part * statement_part option
                  | CmdWhile of expression * statement_part
                  | CmdFor of variable * expression * expression * statement_part
                  | CmdCase of variable * cases list * statement_part option
                  | Statement of statement

and variables = variable list
and variable = VarSimples of id
               | VarCampo of variable * id

and expression = ExpVar of variable
                | ExpInt of int 
                | ExpString of string
                | ExpBool of bool
                | ExpMen of expression
                | ExpNot of expression
                | ExpReal of float
                | ExpOp of oper * expression * expression
                | Expar of expression
                | ExpChamadaF of statement

and expressions = expression list

and oper = | Mais
           |  Menos
           |  Mult
           |  Div
           |  Mod
           |  Menor
           |  Igual
           |  MenorIgual
           |  MaiorIgual
           |  Maior
           |  Difer
           |  And
           |  Or