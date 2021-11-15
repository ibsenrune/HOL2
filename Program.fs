open System
open FParsec
open CharParsers

(* Types *)
type Parser<'t> = Primitives.Parser<'t,unit>
type Expression =
  | LetIn of string * Expression * Expression
  | Add of Expression * Expression
  | Subtract of Expression * Expression
  | Multiply of Expression * Expression
  | Divide of Expression * Expression
  | Integer of int
  | Identifier of string

let curry f = fun x -> fun y -> f(x,y)

let token p = p .>> spaces
let tokenStr str = token (skipString str)
let consume c = token (skipChar c)
let pIdentifier =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier")
let pIdentifierExpr = pIdentifier |>> Identifier
let rec sum s = s |> chainl1 product (choice [consume '+' >>% curry Add; consume '-' >>% curry Subtract])
and sumP : Parser<Expression,unit> = sum
and product s = s |> chainl1 atom    (choice [consume '*' >>% curry Multiply; consume '/' >>% curry Divide])
and atom s = s |> choice [ letIn; consume '(' >>. sum .>> consume ')'; integer; pIdentifierExpr ]
and integer =
  let number : Parser<NumberLiteral> = numberLiteral (NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign) "number"
  let isInteger (i : NumberLiteral) = if i.IsInteger then preturn (System.Int32.Parse(i.String)) else fail (sprintf "Not an integer: %s" i.String)
  number >>=? isInteger |>> Integer
and letIn =
  let lhs = (tokenStr "let") >>. pIdentifier .>> spaces .>> tokenStr "="
  let valueExpression = sumP .>> spaces .>> tokenStr "in"
  pipe3
    lhs
    valueExpression
    sumP
    (fun x y z -> LetIn(x, y, z))

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
  test (sum .>> eof) "let foo = 4 in foo*4" |> output
  0 // return an integer exit code
