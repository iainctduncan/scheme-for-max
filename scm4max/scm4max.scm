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
  

(define handle
  (lambda (args)
    (post (string-append "handle, arg count: " (number->string (length args))))))    

(define adder
  (lambda (args)
    ;;(set! debug-args args)
    (post (string-append "adder arg count: " (number->string (length args))))
    (apply + args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s4m-listeners (make-hash-table))

(define s4m-add-listener
  (lambda (sym fun) 
    (post (string-append "add-listener " (symbol->string sym) ))
    (set! (s4m-listeners sym) fun)))

(define s4m-dispatch
  (lambda args
    (post (string-append "s4m-dispatch arg count: " (number->string (length args))))
    ;;(set! debug-args args)
    ((s4m-listeners (car args)) (cdr args))))

;; register our listeners
(s4m-add-listener 'handle handle)
(s4m-add-listener 'adder adder)

(post "bootstrap complete")

