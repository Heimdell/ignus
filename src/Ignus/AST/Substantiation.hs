
module Ignus.AST.Substantiation where

import Ignus.AST.Definition

alphaEqual :: Expr -> Expr -> Bool
x `alphaEqual` y = case (x, y) of

    (f :@ x, g :@ y) -> True
        && f `alphaEqual` g 
        && x `alphaEqual` y

    (Universe k, Universe i) ->
        k == i

    (Var x, Var y) ->
        x == y

    -- for both functional cases we rename a variable in the second expression
    ((a, a') :-> b, (c, c') :-> d) -> True
        && a' `alphaEqual` c'
        && b  `alphaEqual` (d `substantiate` (c, Var a))

    ((a, a') :=> b, (c, c') :=> d) -> True
        && a' `alphaEqual` c'
        && b  `alphaEqual` (d `substantiate` (c, Var a))

    _ ->
        False

substantiate :: Expr -> (Name, Expr) -> Expr
body `substantiate` (name, value) =
    case body of
        f :@ x -> 
            recure f :@ recure x

        Universe k ->
            body

        Var name' ->
            case name == name' of
                True  -> value
                False -> body

        (a, a') :-> b ->
            substantiateDep (:->) a a' b

        (a, a') :=> b ->
            substantiateDep (:=>) a a' b

  where
    -- To evade name clash we inserting a new var in place of function arg
    --  along its body.
    substantiateDep (~>) a a' b =
        let 
            c = if a == name then refresh a else a

            d = b `substantiate` (a, Var c)

        in (c, recure a') ~> recure d

    recure = (`substantiate` (name, value))
