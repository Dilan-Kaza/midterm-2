# CMSC 430 Midterm 2, Part 3

## Instructions

You've been provided a modified implementation of the Hoax language that was
presented in class.

In Hoax, we implemented strings, but (like lists and vectors) we didn't add many
operations _on_ strings. Your job is to complete or repair the implementation of
the following string-related functions from Racket:

  * `string->list`

    - The syntax is `(string->list e)`.
    - The `e` should evaluate to a string _s_.
    - The operation takes a string as argument and decomposes that string into a
      list of characters; the resulting list should have the same length as the
      number of characters in the input string.

    ```
    > (string->list "hello")
    '(#\h #\e #\l #\l #\o)
    > (string->list "")
    '()
    ```

  * `string-copy`

    - The syntax is `(string-copy e)`.
    - The `e` should evaluate to a string _s_.
    - The operation takes a string as argument and returns a new, identical
      string.

    ```
    > (string-copy "hello")
    "hello"
    > (let ([s1 "hello"])
        (let ([s2 (string-copy s1)])
          (eq? s1 s2)))
    #f
    ```

  * `string-append`

    - The syntax is `(string-append e ...)`.
    - Each `e` that is given should evaluate to a string _s-i_.
    - The operation returns a new string created by composing the characters
      that constitute each of the argument strings in order.
    - If no arguments are given, the empty string is returned.

    ```
    > (string-append "Hello" ", " "World" "!")
    "Hello, World!"
    > (string-append)
    ""
    ```

The AST types, parser, and interpreter have been updated to implement these new
forms correctly. The compiler implementations for these forms are either
incomplete or are faulty.

Fix the implementation of functionality in `compile-ops.rkt` so that
`string->list`, `string-copy`, and `string-append` work correctly.

## Notes

  * A few tests have been added.

  * The `list` and `vector` forms have been implemented for you as conveniences
    (but we don't think these are necessary or specifically useful for this
    problem).
