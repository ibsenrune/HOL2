module Expression

open Ast

let rec closedIn (e : Expression) (vs : string list) : bool =
  match e with
  | IfThenElse(p, b1, b2) -> closedIn p vs && closedIn b1 vs && closedIn b2 vs
  | Call(f, a) -> closedIn f vs && closedIn a vs
  | Function(p, e) -> closedIn e (p::vs)
  | LetIn(x, erhs, ebody) -> closedIn erhs vs && closedIn ebody (x::vs)
  | Add(e1, e2)
  | Subtract(e1, e2)
  | Multiply(e1, e2)
  | Divide(e1, e2) -> closedIn e1 vs && closedIn e2 vs
  | Integer(_) -> true
  | Identifier(s) -> List.contains s vs

let closed e = closedIn e []
let rec union xs ys = List.append xs ys |> List.distinct
let rec except xs ys = List.except ys xs

let rec freeVars (e : Expression) : string list =
  match e with
  | IfThenElse(p,e1,e2) -> (freeVars p) |> union (freeVars e1) |> union (freeVars e2)
  | Call(f,a) -> (freeVars f) |> union (freeVars a)
  | Function(p, b) -> (freeVars b) |> except [p]
  | LetIn(v, erhs, b) -> (freeVars erhs) |> union (freeVars b |> except [v])
  | Add(e1, e2)
  | Subtract(e1, e2)
  | Multiply(e1, e2)
  | Divide(e1, e2) -> (freeVars e1) |> union (freeVars e2)
  | Integer _ -> []
  | Identifier s -> [s]

//let rec subst value body expression =
