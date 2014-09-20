
module Test where

import Ignus

identity_function = 
    (name "A", Universe 0) :-> 
    (name "a", var "A")    :-> 
        var "a"

test4 = normalize $ nat_S

nat_0 = 
    (name "B",       Universe 0) :->
    (name "initial", var "B")    :->
    (name "next",   (Dummy, var "B") :=> var "B") :->
        var "initial"

nat_S = 
    (name "B",       Universe 0) :->
    (name "initial", var "B")    :->
    (name "next",   (Dummy, var "B") :=> var "B") :->
        (var "next" :@ (identity_function :@ var "B" :@ var "initial"))
