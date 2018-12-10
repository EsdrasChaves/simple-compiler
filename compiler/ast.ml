open Lexing

type id = string
type 'a pos =  'a * Lexing.position


type 'expr programa = Program of (id pos) * declarations * ('expr function_declarations) * ('expr statements)


and declarations = declaration list 
and 'expr function_declarations = ('expr function_declaration) list
and 'expr statements = ('expr statement) list

and declaration = DecVar of (id pos) * tipo

and 'expr function_declaration = Function of ('expr decfun)

and 'expr decfun = {

  fn_nome:    id pos;
  fn_tiporet: tipo;
  fn_formais: (id pos * tipo) list;
  fn_locais:  declarations;
  fn_corpo:   'expr statements
}


and tipo = | TypeInteger
           | TypeReal
           | TypeBoolean
           | TypeString
           | TipoVoid

and 'expr statement =   
                | CmdAtrib of 'expr * 'expr
                | CmdIf of 'expr * ('expr statements) * ('expr statements option)
                | CmdRead of ('expr expressions)
                | CmdWrite of ('expr expressions)
                | CmdReadLn of ('expr expressions)
                | CmdWriteLn of ('expr expressions)
                | CmdWhile of 'expr * ('expr statements)
                | CmdFor of 'expr * 'expr * 'expr * ('expr statements)
                | CmdCase of 'expr * ('expr cases list) * ('expr statement option )
                | CmdRetorno of 'expr option
                | CmdFunctionCall of 'expr


and 'expr cases = Case of 'expr * ('expr statement)


                  

and 'expr variables = ('expr variable) list
and 'expr variable = VarSimples of id pos
               | VarCampo of ('expr variable) * (id pos)

and 'expr expressions = 'expr list


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