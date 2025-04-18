# CMSC 430 Midterm 1, Part 4

## Instructions

In Iniquity, we introduced user-defined functions with the `define` form:

```
(define (name p1 ...) e)
```

where `name` is the function name, `p1 ...` are parameter names, and `e` is the
body expression of the function in which the parameters will eventually be bound
to arguments at run-time.

Racket (like many other languages) gives programmers the ability to define
functions that have some parameters that can take on *default values* when not
supplied by the function's caller. The syntax for this new-and-improved `define`
form looks like:

```
(define (name rp1 ... [dp1 de1] ...) e)
```

where `name` is the function name, `rp1 ...` are the *required parameter* names
(which are just what we had before), `dp1 ...` are the *default parameter*
names, `de1 ...` are the default parameters' *default argument expressions*, and
`e` is the function body expression in which all the parameters will eventually
be bound to arguments at run-time.

The difference between the regular Iniquity `define` and our new `define` is
that we can now optionally omit or provide some arguments in a function call:

```
> (define (f x [y 1] [z 2]) (+ x (+ y z)))
> (f 0)
3
> (f 1 2)
5
> (f 1 2 3)
6
```

You've been provided a modified implementation of the Iniquity language that was
presented in class. All parts of the implementation have been updated to support
functions with default parameters; you are not going to implement any code.
Instead, your job is to answer a series of questions about the code,
demonstrating your skills in code comprehension and reasoning.

Edit the file `responses.rkt` in the same directory as this `README.md` file to
answer the questions.

## Notes

  * For each question, refer to the corresponding numbered `TODO` comment in the
    code to formulate your answer.

  * You will be penalized for incorrect answers, so if you are completely unsure
    of the correct choice, it may be more beneficial to leave the question
    unanswered.

  * Some questions may have multiple correct responses. You should indicate all
    of them. (See below for instructions on formatting.)

## Extra Notes on Formatting Responses

Questions in `responses.rkt` have the following syntax:

```
;; N. question text
(define-multiple-choice name
  ([option-name option-description] ...)

  response)
```

where `N` is the unique number identifying the question, `question text` is the
actual question itself, `name` is an identifier used to export your response for
the autograder, `[option-name option-description]` is an identifier--string pair
that describes a possible answer for the question, and `response` is your
answer, formulated in one of three ways:

  * `(no-answer)` indicates you wish to skip the question, i.e., no points will
    be given or taken for that question.

  * `(answer option-name)` indicates a single-answer response, designating
    `option-name` as your selection.

  * `(answers option-name1 option-name2 ...)` indicates a multiple-answer
    response, designating `option-name1 option-name2 ...` as your selections.

Some examples:

```
;; 0. This compilers class is...
(define-multiple-choice ex-1
  ([a "Very fun!"]
   [b "Just the worst."])

  (answer a))

;; 42. My favorite registers are...
(define-multiple-choice ex-2
  ([a "rax"]
   [b "rbx"]
   [c "rsp"])

  (answers a b c))

;; 2001. In the film "2001: A Space Odyssey", the computer assistant HAL-9000 is
;; famous for uttering this phrase.
(define-multiple-choice ex-3
  ([a "Game over, man, game over!"]
   [b "I'd really love to do that, Dave."]
   [c "Muuuuurph! :("]
   [d "*R2D2 noises*"])

  (no-answer))
```

Note that changing the text of the questions or the responses will not help you,
since we were smart enough to make backups before distributing the exam.
