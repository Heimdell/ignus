
module Ignus.Eval where

import Ignus.AST
import Ignus.C

{-
    Infer a type of expression.
-}
typeOf :: Expr -> C Expr
typeOf expr = ["typechecking", show expr] `decorateError` 
    case expr of
        f :@ x -> do
            [x', f'] <- mapM typeOf [x, f]

            case f' of             
                (a, a') :=> b' -> do

                    (a' `alphaEqual` x')
                        `assert` printf "Cannot apply (%s: %s) to %s: %s is %s =/= required %s"
                            (show f)
                            (show f')
                            (show x)
                            (show x)
                            (show x')
                            (show a')

                    a `hasType` x |- normalize b'

                _ ->
                    die $ printf "Cannot give %s to %s: latter has non-functional type %s"
                        (show x)
                        (show f)
                        (show f')

        Universe k -> 
            return $ Universe (k + 1)

        Var name -> 
            lookupVarType name

        (a, a') :-> b -> do

            -- if the `a'` is some invalid object, it will crash here
            -- (the result is ignored)
            typeOf a'

            b' <- a `hasType` a' |- typeOf b

            return $ (a, a') :=> b'

        (a, a') :=> b -> 
            maxUniverse a' b

{-
    Infer a level (universe) of expression.
-}
universeOf :: Expr -> C Expr
universeOf expr = ["finding universe of", show expr] `decorateError` 
    case expr of
        f :@ x ->
            maxUniverse f x

        Universe k ->
            return $ Universe (k + 1)

        Var name -> do
            type_ <- lookupVarType name
            
            universeOf type_

        (a, a') :-> b ->
            universeOfDep a a' b

        (a, a') :=> b ->
            universeOfDep a a' b

  where
    -- functions and pi-types threated undistinctively here
    universeOfDep a a' b = do
        Universe a'' <-                   universeOf a'
        Universe b'' <- a `hasType` a' |- universeOf b

        return $ Universe $ max a'' b''

maxUniverse :: Expr -> Expr -> C Expr
maxUniverse x y = do
    Universe k <- universeOf x
    Universe i <- universeOf y
    
    return $ Universe $ max k i

normalize :: Expr -> C Expr
normalize expr = ["evaluating", show expr] `decorateError` 
    case expr of
        f :@ x -> do
            f' <- normalize f
            x' <- normalize x

            return (x' ==> f')

        Universe k ->
            return expr

        Var name -> do
            result <- lookupVarValue name

            case result of
                Nothing ->
                    return expr

                Just other ->
                    normalize other

        (a, a') :-> b -> 
            normalizeDep (:->) a a' b

        (a, a') :=> b -> 
            normalizeDep (:=>) a a' b

  where
    x ==> f = case f of
        (arg, type_) :-> body ->
            body `substantiate` (arg, x)

        (arg, type_) :=> body ->
            body `substantiate` (arg, x)

        g :@ y ->
            (x ==> (y ==> g))

        Var _ ->
            f

        Universe _ ->
            f

    normalizeDep (~>) a a' b = a `hasType` a' |- do
        a'' <- normalize a'
        b'  <- normalize b
            
        return $ (a, a'') ~> b'

