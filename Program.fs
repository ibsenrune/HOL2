open System
open Ast
open Parser

let output = function
| Result.Ok(r) -> printfn "%A" r
| Result.Error(msg) -> printfn "%s" msg
  
[<EntryPoint>]
let main argv =
  //let ast = parse program "let f = fun x -> fun y -> x*y in f 3 4"
  let ast = parse program "let f = fun x -> x*4 in f 3"
  let res = Result.map (Interpreter.interpret') ast
  printfn "%A" res
  0 // return an integer exit code
