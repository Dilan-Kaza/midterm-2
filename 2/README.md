# CMSC 430 Midterm 2, Part 2

## Instructions

You've been provided a modified implementation of the Hoax language that was
presented in class.

In Hoax we implemented vectors, but (like lists) we didn't add many operations
_on_ vectors. Your job is to implement the `vector-append` function from Racket,
but restricted to only accept two arguments. These arguments must evaluate to
vectors, and the result is a (new) vector combining all the elements of those
two argument vectors in order.

Here is an example:

```
> (vector-append (vector 1 2 3) (vector 4 5 6))
'#(1 2 3 4 5 6)
```

Your `vector-append` function should work similarly to Racket's:

  * The syntax is `(vector-append e e)`.
  * Each `e` should evaluate to a vector _v_.
  * The operation creates and returns a new vector containing the elements that
    were in each of the argument vectors.

The AST types, parser, and interpreter have been updated to implement this new
form, and a stub has been added to the compiler. You must finish the compiler
implementation.

Implement the functionality in `compile-ops.rkt` so that `vector-append` works.

## Notes

  * A few very simple tests have been added.

  * The `list` and `vector` forms have been implemented for you as conveniences.
