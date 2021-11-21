open System
open Ast
open Parser

let output = function
| Result.Ok(r) -> printfn "%A" r
| Result.Error(msg) -> printfn "%s" msg
  
[<EntryPoint>]
let main argv =
  test program "let f = fun x -> fun y -> x*y in f 3 4" |> output
  0 // return an integer exit code
