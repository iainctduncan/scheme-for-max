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

;; convenience functions for output
(define (out outlet_num args) (max-output outlet_num args))
(define (out-0 args) (max-output 0 args))
(define (out-1 args) (max-output 1 args))
(define (out-2 args) (max-output 2 args))
(define (out-3 args) (max-output 3 args))
(define (out-4 args) (max-output 4 args))
(define (out-5 args) (max-output 5 args))
(define (out-6 args) (max-output 6 args))
(define (out-7 args) (max-output 7 args))

(post "scm4max.scm BOOTSTRAP COMPLETE")


