ignus
=====

The variant (dialect) for morte language implementation.

Implementation based on
    http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/

    and the next 2 posts.

This is a toy compiler of dependently-typed lambda calculus -
 a programming language with next constructs:
  - f x          = apply function `f` to argument `a`

  - universe n   = univ-0     is a type of plain type (Int, String, etc)
                   univ-(n+1) is a type of univ-n-level object

  - var x        = referencing a variable called `x`

  - (a : A) -> b = an unnamed function from variable `a` of type `A`
                   to expression `b`. A `b` could contain refs to `a`.

  - (a : A) => b = a type of function from `A` to `b`.
                   Note, `b` could be dependent on `a` - there is
                   the "dependecy" come from.
