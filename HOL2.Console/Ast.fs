module Ast

type Expression =
  | IfThenElse of Expression * Expression * Expression
  | Call of Expression * Expression
  | Function of param : string * body : Expression
  | LetIn of string * Expression * Expression
  | Add of Expression * Expression
  | Subtract of Expression * Expression
  | Multiply of Expression * Expression
  | Divide of Expression * Expression
  | Integer of int
  | Identifier of string
