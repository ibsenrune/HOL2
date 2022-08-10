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

let readProgram = function
  | [| path |] when System.IO.File.Exists(path) -> Some(System.IO.File.ReadAllText(path)) 
  | _ -> None

let printHelp unit : unit = printfn("No file provided")
  
[<EntryPoint>]
let main argv =
  match readProgram argv with
  | None -> printHelp ()
  | Some(text) ->
      let ast = parse program "let f = fun x -> 2*x in let g = fun y -> 3*y in f (g 4)"
      let res = Result.bind Interpreter.interpret' ast
      printfn "%A" res
  0 // return an integer exit code
