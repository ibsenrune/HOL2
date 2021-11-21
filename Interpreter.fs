module Interpreter
open Ast

type Value =
  | Integer of int
  | Function of string * Expression
type Environment = Map<string, Value>
let interpret e =
  let rec inter (env : Environment) (expr : Expression) =
    let interEnv = inter env
    let binaryArithmetic op lhs rhs =
      match interEnv lhs, interEnv rhs with
      | Integer x, Integer y -> Integer(op x y)
      | _ -> failwith "Arithmetic operations are not supported for non-integer types"
    match expr with
    | Expression.Function(param, e) -> Value.Function(param, e)
    | Appl(f,a) ->
        match f with
        | Expression.Function(p,e) -> inter env (LetIn(p, a, e))
        | _ -> failwith (sprintf "%A is not callable" f)
    | LetIn(name, e, body) ->
        let value = inter env e
        let env' = Map.add name value env
        inter env' body
    | Add(lhs, rhs) -> binaryArithmetic (+) lhs rhs
    | Subtract(lhs, rhs) -> binaryArithmetic (-) lhs rhs
    | Multiply(lhs, rhs) -> binaryArithmetic (*) lhs rhs
    | Divide(lhs, rhs) -> binaryArithmetic (/) lhs rhs
    | Expression.Value(Primitive.Integer i) -> Value.Integer i
    | Expression.Value(Identifier id) -> Map.find id env
  inter (Map.empty) e

