open System
open Ast
open Expression
open Parser

let output = function
| Result.Ok(r) -> printfn "%A" r
| Result.Error(msg) -> printfn "%s" msg

let getOk = function
| Result.Ok(r) -> r
| Result.Error(msg) -> failwith (sprintf "Failed: %s" msg)
  
[<EntryPoint>]
let main argv =
  let ast = parse program "((fun x -> fun y -> x*y) 3 4)+2" //virker ikke
  let res = Result.bind Interpreter.interpret' ast
  //let res = closedIn 
  //            (LetIn("x", Integer 3, Multiply ((Identifier "x"), (Identifier "x"))))
  //            ["foo"]
  //let res = closedIn ast []
  printfn "%A" res
  0 // return an integer exit code
