;; the scm4max scheme code to build the API


(define (stringify item)
  (cond
    ((symbol? item) (string-append "'" (symbol->string item)))
    ((number? item) (number->string item))
    ((string? item) item)
    (else "<unhandled type>"))) 

(define (post . args)
  (letrec (
    (log-string (lambda (lat)
                  (cond 
                    ((null? lat) "")
                    ((list? (car lat)) (string-append "<list: " (log-string (car lat)) (log-string (cdr lat)) ">")) 
                    (else (string-append (stringify (car lat)) " " (log-string (cdr lat))))))))
    (max-post (log-string args))))
  

(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s4m-listeners (make-hash-table))

(define s4m-listen
  (lambda (sym fun) 
    (post (string-append "add-listener " (symbol->string sym) ))
    (set! (s4m-listeners sym) fun)))

(define s4m-dispatch
  (lambda args
    (post "s4m-dispatch :" (car args) (cdr args))
    ;;(set! debug-args args)
    ((s4m-listeners (car args)) (cdr args))))


(post "bootstrap complete")

