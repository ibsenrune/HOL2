module Interpreter
open Ast

type Value =
  | Integer of int
  | Function of string * Expression
type Environment = Map<string, Value>
let rec interpret (env : Environment) (expr : Expression) =
  let interEnv = interpret env
  let binaryArithmetic op lhs rhs =
    match interEnv lhs, interEnv rhs with
    | Integer x, Integer y -> Integer(op x y)
    | _ -> failwith "Arithmetic operations are not supported for non-integer types"
  match expr with
  | Call(f,a) ->
      match f with
      | Expression.Identifier(i) ->
          match Map.tryFind i env with
          | Some(Value.Function(p,e)) -> interpret env (LetIn(p, a, e))
          | _ -> failwith (sprintf "Unkonwn function %s" i)
      | Expression.Function(p,e) -> interpret env (LetIn(p, a, e))
      | _ -> failwith (sprintf "%A is not callable" f)
  | Expression.Function(param, e) -> Value.Function(param, e)
  | LetIn(name, e, body) ->
      let value = interpret env e
      let env' = Map.add name value env
      interpret env' body
  | Add(lhs, rhs) -> binaryArithmetic (+) lhs rhs
  | Subtract(lhs, rhs) -> binaryArithmetic (-) lhs rhs
  | Multiply(lhs, rhs) -> binaryArithmetic (*) lhs rhs
  | Divide(lhs, rhs) -> binaryArithmetic (/) lhs rhs
  | Expression.Integer i -> Value.Integer i
  | Expression.Identifier id -> Map.find id env
let interpret' = interpret (Map.empty)
