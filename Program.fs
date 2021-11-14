open System
open FParsec
open CharParsers

(* Types *)
type Parser<'t> = Primitives.Parser<'t,unit>
type Expression =
  | Add of Expression * Expression
  | Subtract of Expression * Expression
  | Multiply of Expression * Expression
  | Divide of Expression * Expression
  | Integer of int

let curry f = fun x -> fun y -> f(x,y)

let token p = p .>> spaces
let consume c = token (skipChar c)
let rec sum s = s |> chainl1 product (choice [consume '+' >>% curry Add; consume '-' >>% curry Subtract])
and product s = s |> chainl1 atom    (choice [consume '*' >>% curry Multiply; consume '/' >>% curry Divide])
and atom s = s |> choice [ consume '(' >>. sum .>> consume ')'; integer ]
and integer : Parser<Expression> =
  let number : Parser<NumberLiteral> = numberLiteral (NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign) "number"
  let isInteger (i : NumberLiteral) = if i.IsInteger then preturn (System.Int32.Parse(i.String)) else fail (sprintf "Not an integer: %s" i.String)
  number >>=? isInteger |>> Integer

(* Parsers *)
let test p str =
  match run p str with
  | Success(result, _, _) -> Result.Ok(result)
  | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

let output = function
| Result.Ok(r) -> printfn "%A" r
| Result.Error(msg) -> printfn "%s" msg
  
[<EntryPoint>]
let main argv =
  //test sum "20+(21+3)*3" |> output
  test sum "1+2+3" |> output
  0 // return an integer exit code
