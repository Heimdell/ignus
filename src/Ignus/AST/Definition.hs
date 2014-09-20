
module Ignus.AST.Definition where

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

refresh :: Name -> Name
refresh name = case name of
    String s   -> Fresh  s  0
    Fresh  s n -> Fresh  s (n + 1)
    Dummy      -> Fresh "_" 0

var  = Var . String
name =       String
