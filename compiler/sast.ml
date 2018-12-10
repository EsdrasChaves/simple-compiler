open Ast


type expression = ExpVar of (expression variable)
                | ExpInt of int pos
                | ExpString of string pos
                | ExpBool of bool pos 
                | ExpReal of float pos 
                | ExpOp of oper pos * expression * expression
                | ExpChamadaF of id pos * (expression expressions)