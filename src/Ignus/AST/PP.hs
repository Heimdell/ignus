
module Ignus.AST.PP where

import Ignus.AST.Definition

instance Show Name where
    show name = case name of
        String s   ->  s
        Fresh  s n ->  s ++ show n
        Dummy      -> "_"

instance Show Expr where
    show = show . convert

data ASTForShow
    = ShowApp     [ASTForShow]
    | ShowUniverse Int
    | ShowVar      String
    | [(String, ASTForShow)] :- ASTForShow
    | [(String, ASTForShow)] := ASTForShow

convert :: Expr -> ASTForShow
convert ast = case ast of 
    _ :@ _ ->
        collectApp ast

    Universe k ->
        ShowUniverse k

    Var name ->
        ShowVar (show name)

    _ :-> _ ->
        collectLambdas ast

    _ :=> _ ->
        collectPis ast

  where
    collectApp (f :@ x) =
        ShowApp (rest ++ [convert x])

      where
        ShowApp rest = collectApp f

    collectApp x = ShowApp [convert x]

    collectLambdas ((name, type_) :-> body) =
        ((name', convert type_) : restArgs) :- restBody

      where
        name' = show name

        restArgs :- restBody = collectLambdas body

    collectLambdas ast =
        [] :- convert ast

    collectPis ((name, type_) :=> body) =
        ((name', convert type_) : restArgs) := restBody

      where
        name' = show name

        restArgs := restBody = collectPis body

    collectPis ast =
        [] := convert ast

instance Show ASTForShow where
    show ast = case ast of
        ShowApp list -> 
            unwords (map showAvareOfAtomics list)

        ShowUniverse n ->
            "Set" ++ show n

        ShowVar name ->
            name

        args :- body -> ""
            ++ unwords (map showArg args)
            ++ " -> "
            ++ show body

        args := body -> ""
            ++ unwords (map showArg args)
            ++ " => "
            ++ show body

      where
        showArg (name, arg) = case name of
            "_" ->
                show arg
            
            _ ->
                "(" ++ name ++ " : " ++ show arg ++ ")"

        showAvareOfAtomics expr
            | atomic expr =
                show expr

            | otherwise =
                concat ["(", show expr, ")"]

        atomic expr = case expr of
            ShowVar      _ -> True
            ShowUniverse _ -> True
            _              -> False

