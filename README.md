ignus
=====

The variant (dialect) for morte language implementation.

Implementation based on http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/ and the next 2 posts.

This is a toy compiler of dependently-typed lambda calculus - a programming language with the next constructs:
* `f x` - apply a function `f` to argument `x`;

* `universe n` - `universe 0` is a type of all plain types (`Int`, `String`, etc), `universe (n+1)` is a type of `universe n`;

* `var x`

* `(a : A) -> b` - an lambda function from variable `a` of type `A` to expression `b` - a `b` ~~could~~ should contain refs to `a`.

* `(a : A) => b` - a type of functions from `A` to `b` (pi-type). Note, `b` could mention `a` - there is the "dependency" comes from.
