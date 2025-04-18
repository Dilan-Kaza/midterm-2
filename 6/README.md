# CMSC 430 Midterm 2, Part 6

## Instructions

You've been provided a modified implementation of the Hustle language that was
presented in class.

In Hustle we implemented lists, but we didn't add many operations _on_ lists.
Your job is to implement the `append` function from Racket, but restricted to
only accept two arguments. These arguments must evaluate to each be a list, and
the result is a list combining all of the elements of those lists in their
original order.

Here are some examples:

```
> (append (list 1 2) (list 3 4))
'(1 2 3 4)
> (append (list) (list))
'()
> (append (list (list 1 2) 3) (list 4 5))
'((1 2) 3 4 5)
```

Your `append` should work similarly to Racket's:

  * The syntax is: `(append e e)`.
  * Each `e` should evaluate to a list.
  * The operation creates and returns a new list containing the elements that
    were in each of the argument lists.

The AST types, parser, and interpreter have been updated to implement this new
form, and a stub has been added to the compiler. You must finish the compiler
implementation.

Implement the functionality where indicated by `TODO` comments in
`compile-ops.rkt` so that `append` works.

## Notes

  * A few very simple tests have been added.

  * The `list` function has been implemented for you as a convenience.

  * You must stick to the skeleton provided unless otherwise indicated.
