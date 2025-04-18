#lang racket

;; NOTE: You absolutely do not need to read anything in this file; I'd just
;; ignore it if I were you.

(provide define-multiple-choice)

(require syntax/parse/define
         (for-syntax racket/format racket/list racket/syntax))

(begin-for-syntax
  ;; Defines a parameterized syntax class to ensure the name is "defined".
  (define-syntax-class (option-id options)
    (pattern n:id
             #:fail-when (and (not (memq (syntax-e #'n) options))
                              #'n)
             (format "answer must be from available options: (~a)"
                     (apply ~a #:separator ", " options))))
  ;; Defines a syntax class to capture one or more strings together.
  (define-syntax-class strings
    (pattern s:str)
    (pattern (v:str ...+)
             #:with s
             #`#,(apply ~a
                        #:separator " "
                        (map syntax-e (syntax->list #'(v ...)))))))

;; (define-multiple-choice name
;;   ([option-name option-description] ...)
;;   response)
;;
;; name               = id
;; option-name        = id
;; option-description = string
;;
;; response = (no-answer)
;;          | (answer option-name)
;;          | (answers option-name ...+)
(define-syntax-parse-rule (define-multiple-choice name:id
                            ([option-name:id option-desc:strings] ...)
                            (~do (define options (map syntax-e (syntax->list #'(option-name ...)))))
                            (~or* ((~datum no-answer) ~!
                                   (~bind [response #'#f]))
                                  ((~datum answer) ~!
                                   (~and (~var answer (option-id options))
                                         (~bind [response #''answer])))
                                  ((~datum answers) ~!
                                   (~and (~seq (~var answers (option-id options)) ...+)
                                         (~bind [response #''(answers ...)])))
                                  (~and (~rest response)
                                        (~bind [bad-response #t]))))
  #:fail-when (and (attribute bad-response)
                   #'response)
  (let ([options-string (apply ~a #:separator ", " options)])
    (format "Response must be one of:\n    ~a\n    ~a\n    ~a"
            "(no-answer)"
            (format "(answer x) where x is one of ~a" options-string)
            (format "(answers x ...) where each x is one of ~a" options-string)))
  #:fail-when (check-duplicates (syntax->list #'(option-name ...)) eq? #:key syntax-e) "Duplicate option name"
  #:with name-options (format-id #'name "~a-options" #'name)
  (begin (define name response)
         (define name-options '([option-name option-desc.s] ...))
         (provide name-options)))
