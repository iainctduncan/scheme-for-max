; these get set elsewhere and represent values that 
; will expand %1 %2 etc
(define s4m-expr-inputs (vector '() 11 22 33)) 

(define (string->sexp text)
  "convert a string of an sexp to an sexp"
  (read (open-input-string text)))

(define-macro (run-expr sexp)
  (cons 'eval (list
    (map 
      (lambda(token)
        (if (and (symbol? token) (eq? ((symbol->string token) 0) #\%))
          `(s4m-expr-inputs ,(string->number 
              (list->string (cdr (string->list (symbol->string token))))))
          token))
      sexp))))

(define-macro (run-expr* sexp)
  `(eval (map
     (lambda(token)
       (if (and (symbol? token) (eq? ((symbol->string token) 0) #\%))
         `(s4m-expr-inputs ,(string->number (substring (symbol->string token) 1)))
          token))
       ,sexp)))

(define (s4m-run-expr sexp-str)
  (let ((input-sexp (string->sexp sexp-str)))
    (run-expr* input-sexp)))


; desired behaviour
; (s4m-run-expr "(+ %1 %2 %3)")
; should run as (+1 (expr-ins 1) (expr-ins 2) (expr-ins 3))

; error i get:
;make-iterator argument, input-sexp, is a symbol but should be a sequence
;    (cons 'eval (list (map (lambda...


;(post "sexp-test.scm loaded")

