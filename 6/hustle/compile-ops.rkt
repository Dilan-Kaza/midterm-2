#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-opN pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast)

(define rax 'rax)
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in op2
(define r9  'r9)  ; scratch

(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq pad-stack (Call 'peek_byte) unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (Cmp rax 0)
          if-equal)]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          if-equal)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack)]
    ['box
     (seq (Mov (Offset rbx 0) rax) ; memory write
          (Mov rax rbx)            ; put box in rax
          (Xor rax type-box)       ; tag as a box
          (Add rbx 8))]
    ['unbox
     (seq (assert-box rax)
          (Xor rax type-box)
          (Mov rax (Offset rax 0)))]
    ['car
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]
    ['cdr
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]

    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons? (type-pred ptr-mask type-cons)]
    ['box?  (type-pred ptr-mask type-box)]))

;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-lt)]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-equal)]
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Xor rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['append
     (let ((loop  (gensym))
           (close (gensym))
           (done  (gensym)))
       (seq (Pop r8)
            (assert-list r8)            ;; check first arg is list
            (assert-list rax)           ;; check second arg is list

            (Cmp r8 (value->bits '()))
            (Je done)                   ;; first arg = '() => return second arg
            (Cmp rax (value->bits '()))
            (Cmove rax r8)
            (Je done)                   ;; second arg = '() => return first arg

            ;; TODO: Do any set-up for the loop that you need.
            ;;
            (Push rax);; HINT: It may be useful to save [rax] until later.
            ;;
            (Mov rax rbx);; HINT: It may be useful to copy the heap pointer for some reason.
            ;;
            ;; NOTE: You are allowed to change any code between here and the
            ;; [(Label done)] instruction at the end.
            
            

            (Push 0)     ;counter
            (Label loop)
            (Mov r9 (value->bits '()))
            (Mov (Offset rbx 0) r9)
            (Xor r8 type-cons)     ;get addy
            (Mov r9 (Offset r8 8)) ;get value
            (Mov (Offset rbx 8) r9);put value in
            (Mov r8 (Offset r8 0)) ;get next cons
            (assert-list r8)
            (Cmp r8 (value->bits '()))
            (Add rbx 16)
            (Mov r9 rbx)
            (Xor r9 type-cons)
            (Mov (Offset rax 0) r9)
            (Add rax 16)
            (Jne loop)

            (Pop r9)                   ;get counter
            (Cmp r9 1)                 ;see if one
            (Je close)                 ;
            (Pop r8)                   ;get second list
            (assert-list r8)
            (Push 1)                   ;push new counter
            (Jmp loop)
            ;; TODO: Implement the loop to build the result list.

            ;; HINT: You may find it useful to have a separate step to finish
            ;; building your result list.
            ;;
            ;; NOTE: If you don't need it, that's fine too.
            (Label close)
            (Sub rax 16)
            (Mov r9 (value->bits '()))
            (Mov (Offset rax 0) r9)
            (Xor rax type-cons)
            ;; TODO ?

            (Label done)))]))

;; Assert that [arg] is really the head of a proper list; error otherwise.
;; NOTE: No registers are overwritten, including [arg].
;; NOTE: You shouldn't need to change this at all.
(define (assert-list arg)
  (let ((loop (gensym))
        (done (gensym)))
    (seq (Push arg)

         (Label loop)
         (Cmp arg (value->bits '()))
         (Je done)
         (assert-cons arg)
         (Xor arg type-cons)
         (Mov arg (Offset arg 0))
         (Jmp loop)

         (Label done)
         (Pop arg))))

;; OpN Natural -> Asm
(define (compile-opN p n)
  (match p
    ['list
     (match n
       [0 (seq (Mov rax (value->bits '())))]
       [_ (seq (Mov r9 rbx)
               (Xor r9 type-cons)
               (compile-op-list n n)
               (Add rsp (* 8 n))
               (Mov rax rbx)
               (Xor rax type-cons)
               (Add rbx (* 16 n)))])]))

;; Natural Natural -> Asm
(define (compile-op-list m n)
  (match n
    [1 (seq (Mov r8 (Offset rsp 0))
            (Mov (Offset rbx (+ (* 16 (sub1 m)) 8)) r8)
            (Mov r8 (value->bits '()))
            (Mov (Offset rbx (* (sub1 m) 16)) r8))]
    [_ (seq (Mov r8 (Offset rsp (* 8 (sub1 n))))
            (Mov (Offset rbx (+ (* 16 (- m n)) 8)) r8)
            (Add r9 16)
            (Mov (Offset rbx (* 16 (- m n))) r9)
            (compile-op-list m (sub1 n)))]))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

;; Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; Asm
;; set rax to #t or #f if comparison flag is less than
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))


;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))
