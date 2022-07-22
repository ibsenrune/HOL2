module Interpreter
open Ast

type Value =
  | Integer of int
  | Closure of string * Expression * Environment
and Environment = Map<string, Value>

let combineEnvironments (inner : Environment) (outer : Environment) : Environment =
  ((Map.toList inner), (Map.toList outer)) ||> List.append |> Map.ofList

let rec interpret (env : Environment) (expr : Expression) : Result<Value,string> =
  //printfn "%A in %A" expr env
  //printfn "----"
  let interEnv = interpret env
  let binaryArithmetic op lhs rhs =
    match interEnv lhs, interEnv rhs with
    | Ok(Integer x), Ok(Integer y) -> Ok(Integer(op x y))
    | Error(s), Ok(_)
    | Ok(_), Error(s) -> Error(s)
    | Error(s), Error(s') -> Error(sprintf "%s %s" s s')
    | _ as e -> Error(sprintf "Arithmetic operations are not supported for non-integer types: %A" e)
  match expr with
  | Call(f, a) ->
      match interpret env f with
      | Ok(Closure(p,b,e)) -> interpret (combineEnvironments e env) (LetIn(p, a, b))
      | _ -> Error (sprintf "%A is not callable" f)
  | Expression.Function(param, e) -> Ok(Value.Closure(param, e, env))
  | IfThenElse(c, e1, e2) ->
    let c' = interEnv c
    match c' with
    | Error(_) -> c'
    | Ok(Integer(0)) -> interEnv e2
    | _ -> interEnv e1
  | LetIn(name, e, body) ->
      let value = interpret env e
      match value with
      | Ok(v) -> 
        let env' = Map.add name v env
        interpret env' body //Her er problemet
      | Error(s) -> Error(s)
  | Add(lhs, rhs) -> binaryArithmetic (+) lhs rhs
  | Subtract(lhs, rhs) -> binaryArithmetic (-) lhs rhs
  | Multiply(lhs, rhs) -> binaryArithmetic (*) lhs rhs
  | Divide(lhs, rhs) -> binaryArithmetic (/) lhs rhs
  | Expression.Integer i -> Ok(Value.Integer i)
  | Expression.Identifier id ->
      match Map.tryFind id env with
      | Some v -> Ok(v)
      | None -> Error (sprintf "Unknown identifier '%s' in environment %A" id env)
let interpret' = interpret (Map.empty)
