#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 compile-opN pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast)

(define rax 'rax)
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in op2
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r11 'r11) ; scratch

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
    ['box?  (type-pred ptr-mask type-box)]
    ['vector? (type-pred ptr-mask type-vect)]
    ['string? (type-pred ptr-mask type-str)]
    ['vector-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-vector rax)
            (Xor rax type-vect)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string->list
     ;; TODO: Fix 2 bugs.
     (let ((loop (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Mov r8 rax)
            (Mov rax (value->bits '()))  ;; rax = result list
            (Xor r8 type-str)            ;; r8 = pointer to string
            (Cmp r8 0)
            (Je done)

            (Mov r9 (Offset r8 0))       ;; r9 = length / number of chars
            (Add r8 8)                   ;; r8 = pointer to first char
            (Mov r10 r9)
            (Sub r10 1)
            (Sal r10 2)                  ;; r10 = number of bytes for chars
            (Add r8 r10)                 ;; r8 = pointer to last char

            (Label loop)
            ;; new cons w/ next char
            (Mov (Offset rbx 0) rax)     ;; put current result list in cdr
            (Mov eax (Offset r8 0))      ;; copy char to rax
            (Sal rax char-shift)         ;; ...
            (Xor rax type-char)          ;; ...
            (Mov (Offset rbx 8) rax)     ;; put char in car
            (Mov rax rbx)                ;; ...
            (Xor rax type-cons)          ;; put cons in rax
            (Add rbx 16)                 ;; ...
            (Sub r8 4)                   ;; r8 = pointer to prev char
            (Sub r9 1)                   ;; r9 = chars remaining
            (Jnz loop)

            (Label done)))]
    ['string-copy
     ;; TODO: Find bug.
     (let ((loop (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Mov r8 rax)
            (Xor r8 type-str)
            (Cmp r8 0)
            (Je done)

            (Mov r10 0)                 ;; r10 = byte offset
            (Mov r9 (Offset r8 0))      ;; r9 = length / loop counter
            (Add r8 8)                  ;; r8 = pointer to first input char
            (Mov (Offset rbx 0) r9)     ;; write length to new string
            (Add rbx 8)

            (Label loop)
            (Mov eax (Offset r8 r10))   ;; load next char into eax
            (Mov (Offset rbx r10) eax)  ;; write char to heap
            (Add r10 4)                 ;; count bytes written
            (Sub r9 1)                  ;; decrement counter
            (Jnz loop)

            (Add r10 #b100)             ;; adds 4
            (Sar r10 3)                 ;;   when length
            (Sal r10 3)                 ;;     is odd

            (Sub rbx 8)
            (Mov rax rbx)
            (Xor rax type-str)          ;; rax = new string
            (Add rbx 8)
            (Add rbx r10)               ;; update heap pointer

            (Label done)))]))

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
    ['make-vector ;; size value
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8) ;; r8 = size
            (assert-natural r8)
            (Cmp r8 0) ; special case empty vector
            (Je empty)

            (Mov r9 rbx)
            (Xor r9 type-vect)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-vect)
            (Label done)))]
    ['vector-ref ; vector index
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp r8 type-vect)
          (Je 'err) ; special case for empty vector
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]
    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)

            (Mov r9 rbx)
            (Xor r9 type-str)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Offset rbx 0) eax)
            (Add rbx 4)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-str)
            (Label done)))]
    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp r8 type-str)
          (Je 'err) ; special case for empty string
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Xor rax type-char))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r8 type-vect)
          (Je 'err)
          (Cmp r10 0)
          (Jl 'err)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'err)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax (value->bits (void))))]))

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
               (Add rbx (* 16 n)))])]
    ['vector
     (match n
       [0 (seq (Mov rax type-vect))]
       [_ (seq (Mov rax n)
               (Mov (Offset rbx 0) rax)
               (compile-op-vector n n)
               (Add rsp (* 8 n))
               (Mov rax rbx)
               (Xor rax type-vect)
               (Add rbx (* 8 (add1 n))))])]
    ;; TODO: Find bug.
    ;; NOTE: Bug can also be in [compile-op-string-append].
    ['string-append
     (match n
       ;; If no args, return empty string.
       [0 (seq (Mov rax type-str))]
       ;; Otherwise, need to copy all chars to new string.
       [_ (seq (Mov r11 8)  ;; r11 = byte offset for new string
               (compile-op-string-append n n))])]))

;; Natural Natural -> Asm
(define (compile-op-string-append m n)
  (match n
    ;; No args left.
    [0 (seq (Mov r10 r11)             ;; copy byte offset
            (Sub r11 8)               ;; r11 = number of bytes written for chars
            (Sar r11 2)               ;; r11 = number of chars written
            (Mov (Offset rbx 0) r11)
            (Mov rax rbx)
            (Xor rax type-str)        ;; rax = new string value
            (Add r10 #b100)           ;; adds 4
            (Sar r10 3)               ;;   when length
            (Sal r10 3)               ;;     is odd
            (Add rbx r10)             ;; update heap pointer
            (Add rsp (* 8 m)))]       ;; update stack pointer
    ;; Process next string in args list.
    [_ (let ((loop (gensym))
             (next (gensym)))
         (seq (Mov r8 (Offset rsp (* 8 (sub1 n))))  ;; get arg from stack
              (assert-string r8)
              (Xor r8 type-str)
              (Cmp r8 0)
              (Je next)

              (Mov r9 (Offset r8 0))  ;; r9 = arg string length
              (Add r8 8)              ;; r8 = pointer to next char
              (Mov r10 0)             ;; r10 = arg string byte offset

              (Label loop)
              (Mov eax (Offset r8 r10))
              (Mov (Offset rbx r11) eax)
              (Add r10 4)             ;; count used/new bytes
              (Add r11 4)             ;;   in offset counters
              (Sub r9 1)
              (Jnz loop)

              (Label next)
              (compile-op-string-append m (sub1 n))))]))

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

;; Natural Natural -> Asm
(define (compile-op-vector m n)
  (match n
    [0 (seq)]
    [_ (seq (compile-op-vector m (sub1 n))
            (Mov r8 (Offset rsp (* 8 (- m n))))
            (Mov (Offset rbx (* 8 n)) r8))]))

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
