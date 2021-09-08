;building on the from Bill:
(define s4m-expr-inputs (vector '() 11 22 33)) 

(define (string->sexp text)
  "convert a string of an sexp to an sexp"
  (read (open-input-string text)))

; recursive iterator for processing an sexp for %X args
(define (s4m-process-sexp sexp)
  (map
    (lambda(token)
      (cond 
        ((and (symbol? token) (eq? ((symbol->string token) 0) #\%))
          `(s4m-expr-inputs ,(string->number (substring (symbol->string token) 1))))
        ((list? token)
          (s4m-process-sexp token))
        (else token)))
     sexp))

(define (s4m-run-expr sexp-str)
  (let ((input-sexp (string->sexp sexp-str)))
    (eval '(eval (s4m-process-sexp input-sexp)))))

; or alternately...
;(define-macro (s4m-run-sexp-macro sexp)
;  `(eval (s4m-process-sexp ,sexp)))
;
;(define (s4m-run-expr sexp-str)
;  (let ((input-sexp (string->sexp sexp-str)))
;    (s4m-run-sexp-macro input-sexp)))


