
{-# LANGUAGE FlexibleContexts #-}

module Core where

import Control.Applicative  ((<*>), (<$>))

import Control.Monad.Reader 
import Control.Monad.Writer
import Control.Monad.State  
import Control.Monad.Error  

import Data.Maybe           (fromMaybe)
import Text.Printf          (printf)

{-
    No use for now. Later will be used for interactive repl.
    You will be able to declare and typecheck objects by one.
-}
data Step
    = Postulate  Type  -- Postulate existence of some object in type.
    | Definition Expr  -- Construct an object of some type (inferrable/forced).

{-
    An AST of language.
-}
data Expr
    = Expr :@ Expr
    | Universe Int          
    | Var Name
    | (Name, Type) :-> Expr  -- a function
    | (Name, Type) :=> Type  -- a pi-type

infixr 9 :->, :=>

type Type = Expr

data Name 
    = String String
    | Fresh  String Int
    | Dummy

    deriving (Eq)

instance Show Name where
    show name = case name of
        String s   ->  s
        Fresh  s n ->  s ++ show n
        Dummy      -> "_"

refresh :: Name -> Name
refresh name = case name of
    String s   -> Fresh  s  0
    Fresh  s n -> Fresh  s (n + 1)
    Dummy      -> Fresh "_" 0

type Env = [(Name, (Type, Maybe Expr))]

{-
    The context of operations.
-}
type C =
    (ErrorT String  -- it could throw an error of text message
    (ReaderT Env
    (Writer [String])))   -- it could provide readonly enviromnent

runC :: C a -> (Either String a, [String])
runC = id 
    . runWriter  
    . flip runReaderT  []  -- initial env is an empty list
    . runErrorT           

{- 
    do or die 
-}
run :: Show a => C a -> IO ()
run action = do
    let (result, log) = runC action
    
    when (not (null log)) $ do
        putStrLn $ "log:\n" ++ unlines (map ("  " ++) log)

    case result of
        Left  err    -> fail  err
        Right result -> print result

{-
    Helpers for tests.
-}
var  = Var . String
name =       String

{-
    A test source for algorithms.

    Represents an identity function, receiving a plain (univ-0) type,
    an object of that type and returning the latter one.
-}
identity_function = (name "A", Universe 0) :-> (name "a", var "A") :-> var "a"

{-
    Run all tests at once.
-}
allTests :: IO ()
allTests = forM_ [test1, test2, test3]
    $ print . runC

{-
    Various tests.
-}
test1, test2, test3 :: C Expr
test1 = return     nat_S
test2 = typeOf     nat_S
test3 = universeOf nat_S

test4 = normalize $ nat_S

nat_0 = 
    (name "A",       Universe 0) :->
    (name "B",       Universe 0) :->
    (name "initial", var "B")    :->
    (name "next",   (Dummy, var "B") :=> var "B") :->
        var "initial"

nat_S = 
    (name "A",       Universe 0) :->
    (name "B",       Universe 0) :->
    (name "initial", var "B")    :->
    (name "next",   (Dummy, var "B") :=> var "B") :->
        (var "next" :@ (identity_function :@ var "B" :@ var "initial"))

coerce :: Expr -> Type -> Expr
coerce expr type_ = 
    ((name "it", type_) :-> var "it") :@ expr

let_ name val type_ expr = ((name, type_) :-> expr) :@ val

{-
    Infer a type of expression.
-}
typeOf :: Expr -> C Expr
typeOf expr = ["typechecking\n\t", show expr, "\n"] `decorateError` case expr of

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

                a `hasType` x $ normalize b'

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


        
        b' <- a `hasType` a' $ typeOf b

        return $ (a, a') :=> b'

    (a, a') :=> b -> 
        maxUniverse a' b

{-
    Infer a level (universe) of expression.
-}
universeOf :: Expr -> C Expr
universeOf expr = ["finding universe of", show expr] `decorateError` case expr of
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
        Universe a'' <-                  universeOf a'
        Universe b'' <- a `hasType` a' $ universeOf b

        return $ Universe $ max a'' b''

maxUniverse :: Expr -> Expr -> C Expr
maxUniverse x y = do
    Universe k <- universeOf x
    Universe i <- universeOf y
    
    return $ Universe $ max k i

{-
    It takes a name and a selector, searches for info about a name
     and applies the selector to the result of search.
-}
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
{-
    Enrich context with a knowledge that `name` hasType `type_`.
-}
hasType :: Name -> Type -> C a -> C a
name `hasType` type_ = 
    local ((name, (type_, Nothing)) :)

normalize :: Expr -> C Expr
normalize expr =  ["evaluating", show expr] `decorateError` case expr of
    f :@ x -> do
        f' <- normalize f   -- go down
        x' <- normalize x
        return (x' ==> f')

    Universe k ->
        return expr

    -- try to lookup a value of the var
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
            (x ==> g) :@ (x ==> y)

        Var _ ->
            f :@ x

        Universe _ ->
            f

    normalizeDep (~>) a a' b = a `hasType` a' $ do
        a'' <- normalize a'
        b'  <- normalize b
            
        return $ (a, a'') ~> b'

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

die = fail

infix 0 `assert`

assert condition message = 
    when (not condition) $ fail message

infixl 1 |>
(|>) = flip ($)

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

for = flip map

decorateError :: MonadError String m => [String] -> m a -> m a 
decorateError doingSmth body = do
    body `catchError` \line ->
        fail (unwords (line : ("\nwhen" : doingSmth)))
