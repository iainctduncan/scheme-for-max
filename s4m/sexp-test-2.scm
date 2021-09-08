
; desired behaviour
; (s4m-run-expr "(+ %1 %2 %3)")
; should run as (+1 (expr-ins 1) (expr-ins 2) (expr-ins 3))

; error i get:
;make-iterator argument, input-sexp, is a symbol but should be a sequence
;    (cons 'eval (list (map (lambda...

; these get set elsewhere and represent values that 
; will expand %1 %2 etc
(define s4m-expr-inputs (vector '() 11 22 33)) 

(define (string->sexp text)
  "convert a string of an sexp to an sexp"
  (read (open-input-string text)))

; fuck doesn't work for nested sexpressions
; probably needs recusion added to it, grrr
(define-macro (run-expr sexp args)
  (post "run-expr, sexp:" sexp "sequence?" (sequence? sexp) "args:" args)
  (cons 'eval (list
    ; loop through tokens in sexp, converting %1 to (args 0)
    (map 
      (lambda(token)
        (post "token:" token)
        (if (and (symbol? token) (eq? ((symbol->string token) 0) #\%))
          ; the below becomes (args 0) on %1, (args 1) on %2, etc.
          (args (- (string->number (list->string (cdr (string->list (symbol->string token))))) 1))
          token))
      sexp))))

; probably needs recusion added to it, grrr
(define-macro (run-expr* sexp args)
  (post "run-expr, sexp:" sexp "sequence?" (sequence? sexp) "args:" args)
  (cons 'eval (list
    ; loop through tokens in sexp, converting %1 to (args 0)
    (map 
      (lambda(token)
        (if (and (symbol? token) (eq? ((symbol->string token) 0) #\%))
          ; the below becomes (args 0) on %1, (args 1) on %2, etc.
          (args (- (string->number (list->string (cdr (string->list (symbol->string token))))) 1))
          token))
      sexp))))

(define (s4m-run-expr sexp-str args)
  (post "s4m-run-expr sexp-str:" sexp-str)
  (let ((input-sexp (string->sexp sexp-str)))
    (eval `(run-expr ,input-sexp ,args))))


;(post "sexp-test.scm loaded")

