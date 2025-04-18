# CMSC 430 Midterm 2, Part 5

## Instructions

You've been provided a modified implementation of the Knock language that was
presented in class.

The `app` pattern applies a (user-defined) function to the value being matched,
and then matches the result of that application against another pattern. For
example:

```
(define (add1car p)
  (add1 (car p)))

(match (cons 1 2)
  [(app add1car 2) #t]
  [_ #f])
```

This will return `#t`: the `add1car` function is applied to the value being
matched, and it will return `2`. This will match the `2` in the pattern. Here's
another example:

```
(define (first-char s) (string-ref s 0))

(define (starts-with-a? s)
  (match s
    [(app first-char #\a) #t]
    [_ #f]))

(starts-with-a? "abc")
```

This program will evaluate to `#t` because the `app` pattern will apply the
string `"abc"` to the user-defined function `first-char` and check that the
result is the character `#\a`, which it is.

Your job is to implement the `app` pattern in both the interpreter and the
compiler. Note that the interpreter code for `interp-match-pat` has been updated
to pass the list of function definitions through for you.

## Notes

  * The AST and parser are implemented correctly.

  * A few simple tests for `app` are provided.
