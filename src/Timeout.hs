module Timeout where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Mod Expr Expr
  | Eq  Expr Expr
  | Neq Expr Expr
  | Gt  Expr Expr
  | Lt  Expr Expr
  | Geq Expr Expr
  | Leq Expr Expr
  | Num Int
  | Var String

data Statement
  = Halt
  | Label String
  | Goto  String
  | If Expr Statement
  | Set String Expr
  | In  String
  | Out Expr
  | Send String Expr Expr Expr


