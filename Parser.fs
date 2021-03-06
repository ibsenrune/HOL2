module Parser
#nowarn "40"

open FParsec
open CharParsers
open Ast

type Parser<'t> = Primitives.Parser<'t,unit>
let keywords = ["let"; "in"; "fun"; "if"; "then"; "else" ]
let curry f = fun x -> fun y -> f(x,y)

let token p = p .>> spaces
let str s = token (skipString s)
let foo = (>>)
let keyword s = spaces .>> (skipString s) .>> spaces
let consume c = skipChar c
let consume_ws c = token (skipChar c)
let isNotKeyword s = if List.contains s keywords then fail (sprintf "'%s' is a reserved word" s) else preturn s
let consumeIdentifier =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  let p = (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier")
  p >>=? isNotKeyword
let identifier = consumeIdentifier |>> Identifier

let rec expr s = s |> choice [ ifThenElse; letIn; call; sum ]
and sum s =
  s |> chainl1
    product
    (choice [
      consume_ws '+' >>% curry Add
      consume_ws '-' >>% curry Subtract])
and product s =
  s |> chainl1
    atom
    (choice [
      consume_ws '*' >>% curry Multiply
      consume_ws '/' >>% curry Divide])
and parenthesized = consume_ws '(' >>. expr .>> consume ')'
and ifThenElse =
  pipe3
    (keyword "if" >>. expr)
    (keyword "then" >>. expr)
    (keyword "else" >>. expr)
    (fun x y z -> IfThenElse(x,y,z))
and call =
  let firstFunc = (parenthesized <|> identifier) .>> spaces1
  let args = sepBy1 expr spaces1 
  let call' = (firstFunc .>>. args) |>> (fun (f, args) -> List.fold (curry Call) f args)
  attempt call'
and atom = choice [ func; parenthesized; integer; identifier ]
and integer =
  let number : Parser<NumberLiteral> = numberLiteral (NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign) "number"
  let isInteger (i : NumberLiteral) = if i.IsInteger then preturn (System.Int32.Parse(i.String)) else fail (sprintf "Not an integer: %s" i.String)
  number >>=? isInteger |>> Integer
and letIn =
  let lhs = (str "let") >>. consumeIdentifier .>> spaces .>> str "=" .>> spaces
  let valueExpression = expr .>> spaces .>> str "in"
  pipe3
    lhs
    valueExpression
    expr
    (fun x y z -> LetIn(x, y, z))
and func =
  pipe2
    (str "fun" >>. consumeIdentifier .>> spaces .>> str "->")
    expr
    (fun x y -> Function(x,y))
and program = expr .>> eof

let parse p str =
  match run p str with
  | Success(result, _, _) -> Result.Ok(result)
  | Failure(errorMsg, _, _) -> Result.Error(errorMsg)
