open Ast

type expression = ExpVar of (expression variable) * tipo
                | ExpInt of int * tipo
                | ExpString of string * tipo
                | ExpBool of bool * tipo
                | ExpVoid
                | ExpReal of float * tipo 
                | ExpOp of (oper * tipo) * (expression * tipo) * (expression * tipo)
                | ExpChamadaF of id * (expression expressions) * tipo