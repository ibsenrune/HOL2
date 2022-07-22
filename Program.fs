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
  //let ast = parse program "let f = fun x -> fun y -> x*y in f 3 4"
  //let ast = parse program "let f = fun x -> fun y -> x*y in f 3 4"
  let ast = parse program "((fun x -> fun y -> x*y) 3 4)+2" //virker ikke
  //let ast = parse program "(fun y -> let x = 3 in x*y) 4" //virker
  //let ast = parse program "let x = 3 in (fun y -> x*y) 4" //virker
  //let ast = parse program "(fun y -> x*y) 4"
  //let ast = parse program "let fac = fun x -> if x then x*(fac (x-1)) else 1 in fac 3"
  //let ast = parse program "if foo then 1 else 2"
  //let ast = parse program "let i = 5 in (fun x -> i*x) 4"
  //let ast = parse program "(fun x -> fun y -> x*y)" |> getOk
  //output ast
  //let res = Result.bind (Interpreter.interpret (Map.ofList [("x", Interpreter.Value.Integer 6)])) ast
  let res = Result.bind Interpreter.interpret' ast
  //let res = closedIn 
  //            (LetIn("x", Integer 3, Multiply ((Identifier "x"), (Identifier "x"))))
  //            ["foo"]
  //let res = closedIn ast []
  printfn "%A" res
  0 // return an integer exit code
