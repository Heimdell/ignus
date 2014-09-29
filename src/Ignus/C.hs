
{-# LANGUAGE FlexibleContexts #-}

module Ignus.C 
    ( module M
    , C
    , Env
    , runC
    , run
    , lookupVarValue
    , lookupVarType
    , hasType
    , die
    , assert
    , printf
    , decorateError
    , (|-)
    , (|>)
    )
where  

import qualified Text.Printf as Printf (PrintfType, printf)

import Control.Monad         as M (when)
import Control.Monad.Reader  as M (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer  as M (Writer,  tell, runWriter)
import Control.Monad.Error   as M (ErrorT, MonadError, throwError, catchError, runErrorT)

import Ignus.AST

type C =
    (ErrorT String
    (ReaderT Env
    (Writer [String]))) -- for debug logs

type Env = [(Name, (Type, Maybe Expr))]

runC :: C a -> (Either String a, [String])
runC = id 
    . runWriter  
    . flip runReaderT  []  -- initial env is an empty list
    . runErrorT           

run :: Show a => C a -> IO ()
run action = do
    let (result, log) = runC action
    
    when (not $ null log) $ do
        putStrLn $ "log:\n" ++ unlines (map ("  " ++) log)

    case result of
        Left  err    -> fail  err
        Right result -> print result

lookupVar :: Name -> C (Type, Maybe Expr)
lookupVar name = do
    env <- ask

    case name `lookup` env of
        Just info -> 
            return info
        
        _ -> 
            die $ printf "Variable %s unknown" (show name)
{-
    Get type of known variable.
-}
lookupVarType :: Name -> C Type
lookupVarType name = do
    (type_, _) <- lookupVar name

    return type_
        
{-
    Get value of known variable.
-}
lookupVarValue :: Name -> C (Maybe Expr)
lookupVarValue name = do
    (_, value) <- lookupVar name

    return value
        
hasType :: Name -> Type -> C a -> C a
name `hasType` type_ = 
    local ((name, (type_, Nothing)) :)

die = fail

infix 0 `assert`

assert condition message = 
    when (not condition) $ fail message

infixl 1 |>
(|>) = flip ($)

printf :: Printf.PrintfType r => String -> r
printf = Printf.printf

decorateError :: MonadError String m => [String] -> m a -> m a 
decorateError doingSmth body = do
    body `catchError` \line ->
        fail (unwords (line : ("\nwhen" : doingSmth)))

infixl 0 |-
(|-) :: (a -> b) -> a -> b
(|-) = ($)