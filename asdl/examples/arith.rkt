#lang planet jmillikan/asdl

module Arith
{
stm = Compound(stm head, stm next) 
    | Assign(identifier id, exp exp) 
    | Print(exp* args)
exp = Id(identifier id) 
    | Num(int v)
    | Op(exp lval, binop bop, exp rval) 
binop = Plus | Minus | Times | Div
}
