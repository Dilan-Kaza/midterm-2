#lang racket

(provide make-test-suite
         make-io-test-suite
         run-tests)

(require rackunit
         rackunit/text-ui)

(define (make-test-suite name run-proc)
  (test-suite
   name

  ;  ;; Abscond examples
  ;  (test-suite
  ;   "Abscond"
  ;   (check-equal? (run-proc 7) 7)
  ;   (check-equal? (run-proc -8) -8))

  ;  ;; Blackmail examples
  ;  (test-suite
  ;   "Blackmail"
  ;   (check-equal? (run-proc '(add1 (add1 7))) 9)
  ;   (check-equal? (run-proc '(add1 (sub1 7))) 7))

  ;  ;; Con examples
  ;  (test-suite
  ;   "Con"
  ;   (check-equal? (run-proc '(if (zero? 0) 1 2)) 1)
  ;   (check-equal? (run-proc '(if (zero? 1) 1 2)) 2)
  ;   (check-equal? (run-proc '(if (zero? -7) 1 2)) 2)
  ;   (check-equal? (run-proc '(if (zero? 0)
  ;                                (if (zero? 1) 1 2)
  ;                                7))
  ;                 2)
  ;   (check-equal? (run-proc '(if (zero? (if (zero? 0) 1 0))
  ;                                (if (zero? 1) 1 2)
  ;                                7))
  ;                 7))

  ;  ;; Dupe examples
  ;  (test-suite
  ;   "Dupe"
  ;   (check-equal? (run-proc #t) #t)
  ;   (check-equal? (run-proc #f) #f)
  ;   (check-equal? (run-proc '(if #t 1 2)) 1)
  ;   (check-equal? (run-proc '(if #f 1 2)) 2)
  ;   (check-equal? (run-proc '(if 0 1 2)) 1)
  ;   (check-equal? (run-proc '(if #t 3 4)) 3)
  ;   (check-equal? (run-proc '(if #f 3 4)) 4)
  ;   (check-equal? (run-proc '(if 0 3 4)) 3)
  ;   (check-equal? (run-proc '(zero? 4)) #f)
  ;   (check-equal? (run-proc '(zero? 0)) #t))

  ;  ;; Dodger examples
  ;  (test-suite
  ;   "Dodger"
  ;   (check-equal? (run-proc #\a) #\a)
  ;   (check-equal? (run-proc #\b) #\b)
  ;   (check-equal? (run-proc '(char? #\a)) #t)
  ;   (check-equal? (run-proc '(char? #t)) #f)
  ;   (check-equal? (run-proc '(char? 8)) #f)
  ;   (check-equal? (run-proc '(char->integer #\a)) (char->integer #\a))
  ;   (check-equal? (run-proc '(integer->char 955)) #\λ))

  ;  ;; Evildoer examples
  ;  (test-suite
  ;   "Evildoer"
  ;   (check-equal? (run-proc '(void)) (void))
  ;   (check-equal? (run-proc '(begin 1 2)) 2)
  ;   (check-equal? (run-proc '(eof-object? (void))) #f))

  ;  ;; Extort examples
  ;  (test-suite
  ;   "Extort"
  ;   (check-equal? (run-proc '(add1 #f)) 'err)
  ;   (check-equal? (run-proc '(sub1 #f)) 'err)
  ;   (check-equal? (run-proc '(zero? #f)) 'err)
  ;   (check-equal? (run-proc '(char->integer #f)) 'err)
  ;   (check-equal? (run-proc '(integer->char #f)) 'err)
  ;   (check-equal? (run-proc '(integer->char -1)) 'err)
  ;   (check-equal? (run-proc '(write-byte #f)) 'err)
  ;   (check-equal? (run-proc '(write-byte -1)) 'err)
  ;   (check-equal? (run-proc '(write-byte 256)) 'err)
  ;   (check-equal? (run-proc '(begin (integer->char 97)
  ;                                   (integer->char 98)))
  ;                 #\b))

  ;  ;; Fraud examples
  ;  (test-suite
  ;   "Fraud"
  ;   (check-equal? (run-proc '(let ([x 7]) x)) 7)
  ;   (check-equal? (run-proc '(let ([x 7]) 2)) 2)
  ;   (check-equal? (run-proc '(let ([x 7]) (add1 x))) 8)
  ;   (check-equal? (run-proc '(let ([x (add1 7)]) x)) 8)
  ;   (check-equal? (run-proc '(let ([x 7]) (let ((y 2)) x))) 7)
  ;   (check-equal? (run-proc '(let ([x 7]) (let ((x 2)) x))) 2)
  ;   (check-equal? (run-proc '(let ([x 7]) (let ((x (add1 x))) x))) 8)
  ;   (check-equal? (run-proc '(let ([x 0])
  ;                              (if (zero? x) 7 8)))
  ;                 7)
  ;   (check-equal? (run-proc '(let ([x 1])
  ;                              (add1 (if (zero? x) 7 8))))
  ;                 9)
  ;   (check-equal? (run-proc '(+ 3 4)) 7)
  ;   (check-equal? (run-proc '(- 3 4)) -1)
  ;   (check-equal? (run-proc '(+ (+ 2 1) 4)) 7)
  ;   (check-equal? (run-proc '(+ (+ 2 1) (+ 2 2))) 7)
  ;   (check-equal? (run-proc '(let ([x (+ 1 2)])
  ;                              (let ([z (- 4 x)])
  ;                                (+ (+ x x) z))))
  ;                 7)
  ;   (check-equal? (run-proc '(= 5 5)) #t)
  ;   (check-equal? (run-proc '(= 4 5)) #f)
  ;   (check-equal? (run-proc '(= (add1 4) 5)) #t)
  ;   (check-equal? (run-proc '(< 5 5)) #f)
  ;   (check-equal? (run-proc '(< 4 5)) #t)
  ;   (check-equal? (run-proc '(< (add1 4) 5)) #f))

  ;  ;; Hustle examples
  ;  (test-suite
  ;   "Hustle"
  ;   (check-equal? (run-proc ''()) '())
  ;   (check-equal? (run-proc '(empty? '())) #t)
  ;   (check-equal? (run-proc '(empty? 3)) #f)
  ;   (check-equal? (run-proc '(empty? (cons 1 2))) #f)
  ;   (check-equal? (run-proc '(box 1)) (box 1))
  ;   (check-equal? (run-proc '(box -1)) (box -1))
  ;   (check-equal? (run-proc '(cons 1 2)) (cons 1 2))
  ;   (check-equal? (run-proc '(unbox (box 1))) 1)
  ;   (check-equal? (run-proc '(car (cons 1 2))) 1)
  ;   (check-equal? (run-proc '(cdr (cons 1 2))) 2)
  ;   (check-equal? (run-proc '(cons 1 '())) (list 1))
  ;   (check-equal? (run-proc '(let ((x (cons 1 2)))
  ;                              (begin (cdr x)
  ;                                     (car x))))
  ;                 1)
  ;   (check-equal? (run-proc '(let ((x (cons 1 2)))
  ;                              (let ((y (box 3)))
  ;                                (unbox y))))
  ;                 3)
  ;   (check-equal? (run-proc '(eq? 1 1)) #t)
  ;   (check-equal? (run-proc '(eq? 1 2)) #f)
  ;   (check-equal? (run-proc '(eq? (cons 1 2) (cons 1 2))) #f)
  ;   (check-equal? (run-proc '(let ((x (cons 1 2))) (eq? x x))) #t))

   ;; Given midterm examples
   (test-suite
    "Midterm"
    (check-equal? (run-proc '(append (list) (list)) )'())
    (check-equal? (run-proc '(append (list) (list 1 2))) '(1 2))
    (check-equal? (run-proc '(append (list 1 2) (list))) '(1 2))
    (check-equal? (run-proc '(append (list 1) (list 2))) '(1 2))
    (check-equal? (run-proc '(append (list 1 2) (list 3 4))) '(1 2 3 4))
    (check-equal? (run-proc '(append (list (list 1 2) 3) (list 4 5))) '((1 2) 3 4 5)))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.
    )))

(define (make-io-test-suite name run/io-proc)
  (test-suite
   name

  ;  ;; Evildoer examples
  ;  (test-suite
  ;   "Evildoer"
  ;   (check-equal? (run/io-proc 7 "") (cons 7 ""))
  ;   (check-equal? (run/io-proc '(write-byte 97) "") (cons (void) "a"))
  ;   (check-equal? (run/io-proc '(read-byte) "a") (cons 97 ""))
  ;   (check-equal? (run/io-proc '(begin (write-byte 97) (read-byte)) "b") (cons 98 "a"))
  ;   (check-equal? (run/io-proc '(read-byte) "") (cons eof ""))
  ;   (check-equal? (run/io-proc '(eof-object? (read-byte)) "") (cons #t ""))
  ;   (check-equal? (run/io-proc '(eof-object? (read-byte)) "a") (cons #f ""))
  ;   (check-equal? (run/io-proc '(begin (write-byte 97) (write-byte 98)) "") (cons (void) "ab"))
  ;   (check-equal? (run/io-proc '(peek-byte)"ab") (cons 97 ""))
  ;   (check-equal? (run/io-proc '(begin (peek-byte) (read-byte))"ab") (cons 97 ""))
  ;   (check-equal? (run/io-proc '(read-byte) "†") (cons 226 "")))

  ;  ;; Extort examples
  ;  (test-suite
  ;   "Extort"
  ;   (check-equal? (run/io-proc '(write-byte #t) "") (cons 'err "")))

  ;  ;; Fraud examples
  ;  (test-suite
  ;   "Fraud"
  ;   (check-equal? (run/io-proc '(let ([x 97]) (write-byte x)) "") (cons (void) "a"))
  ;   (check-equal? (run/io-proc '(let ([x 97])
  ;                                 (begin (write-byte x)
  ;                                        x))
  ;                              "")
  ;                 (cons 97 "a"))
  ;   (check-equal? (run/io-proc '(let ([x 97]) (begin (read-byte) x)) "b")
  ;                 (cons 97 ""))
  ;   (check-equal? (run/io-proc '(let ([x 97]) (begin (peek-byte) x)) "b")
  ;                 (cons 97 "")))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.
    )))
