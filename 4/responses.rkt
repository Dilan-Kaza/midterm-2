#lang racket

(provide mc-1 mc-2 mc-3 mc-4 mc-5 mc-6 mc-7 mc-8 mc-9)

(require "mcdef.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GRADED QUESTIONS
;;
;; For each question below, mark your best response:
;;
;;   * Select (no-answer) if you are not confident in a response.
;;
;;   * Select (answer x), where x is a response option, to respond with the
;;     indicated option.
;;
;;   * Select (answers x y ...), where x and y are valid response options, to
;;     respond with the indicated options.
;;
;; NOTE: Incorrect responses will be penalized, so be careful...
;;
;; NOTE: You should probably be sure to read the [README.md] first...
;;


;; 1. In [compile-app], what is [r8] used for?
(define-multiple-choice mc-1
  ([a "Storing the expressions on the stack."]
   [b "Storing the number of arguments received, computed at run-time."]
   [c "Storing the number of arguments received, computed at compile-time."])

  (answer c))


;; 2. The first action taken by the generated code in [compile-define] is to
;; compare the value in [r8] to... what?
(define-multiple-choice mc-2
  ([a "The number of arguments received."]
   [b "The number of required parameters the function has."]
   [c "The number of default parameters the function has."]
   [d "The total number of parameters the function has."])

  (answer d))


;; 3. The [Je] instruction on this line will jump when...
(define-multiple-choice mc-3
  ([a "The function has received enough arguments for all parameters."]
   [b "The function has received enough arguments for only the required parameters."]
   [c "The function has received enough arguments for only the default parameters."])

  (answer a))


;; 4. The code here generates instructions corresponding to each possible number
;; of received arguments. Describe the sequence of code at this point by
;; indicating all of the correct responses.
(define-multiple-choice mc-4
  ([a1 "For each function label..."]
   [a2 "For each default parameter..."]
   [a3 "For each provided argument..."]

   ;; ...compare the number of received arguments with each number from 0 to n,
   ;; where n is...

   [b1 "...the number of default parameters."]
   [b2 "...the number of provided arguments."]
   [b3 "...the number of arguments received."]
   [b4 "...the number of arguments needed."]

   ;; Jump to a corresponding label when...

   [c1 "...the provided number of arguments requires the compilation of default arguments."]
   [c2 "...the expected number of arguments requires the compilation of default arguments."])

  (answers a1 b4 c1))


;; 5. Under what condition(s) is an error signaled on this line?
(define-multiple-choice mc-5
  ([a "The programmer supplied too many arguments."]
   [b "The programmer supplied too few arguments."]
   [c {"The programmer supplied exactly the right number of arguments, but"
       "forgot to say 'please'."}]
   [d "There were not enough default argument values for the parameters."])

  (answer b))


;; 6. The environment given to [compile-e] is empty. Why?
(define-multiple-choice mc-6
  ([a "This is a bug --- the environment should be built up for each argument."]
   [b "The environment doesn't actually matter, so using an empty one is easy."]
   [c "The arguments are considered 'closed', so the environment must be empty."]
   [d "The function body is always evaluated in an empty environment."])

  (answer c))


;; 7. Consider the following function definition:
;;
;;   (define (f x [y 1] [z (add1 1)])
;;     (+ x (+ y z)))
;;
;; The default argument values are pushed onto the stack in what order?
(define-multiple-choice mc-7
  ([a "In written order, i.e., y goes on the stack first, then z."]
   [b "In reverse order, i.e., z goes on the stack first, then y."])

  (a))


;; 8. Consider the following function definition:
;;
;;   (define (f x [y 1] [z (add1 1)])
;;     (+ x (+ y z)))
;;
;; Describe what happens when the function is called as follows:
;;
;;   (f 1 2 3 4)
(define-multiple-choice mc-8

  ([a {"The 4 is pushed onto the stack as a regular argument, but since the"
       "body doesn't use it, nothing interesting happens."}]

   [b {"The 4 is pushed onto the stack as a regular argument, but since the"
       "body doesn't use it, the stack becomes misaligned."}]

   [c {"The function call is checked for syntactic validity at compile-time,"
       "so this code will fail to compile."}]

   [d {"The 4 is pushed onto the stack as a regular argument, but since the"
       "jump table checks the number of arguments, it will fail at run-time."}])

  (answer c))


;; 9. Consider the following function definition:
;;
;;   (define (g x [y 1] [z (add1 x)])
;;     (write-byte (+ 97 (+ x (+ y z)))))
;;
;; Describe what happens when the function is called as follows:
;;
;;   (f 1 2)
(define-multiple-choice mc-9

  ([a {"The function will fail to compile because the [x] in the default"
       "argument expression of the parameter [z] will not be bound."}]

   [b {"The function will compile, but it will error at run-time because the"
       "[x] in the default argument expression of the parameter [z] will not"
       "be bound."}]

   [c {"The function will compile and run, and the value of [z] will be [2]."}])

  (answer a))
