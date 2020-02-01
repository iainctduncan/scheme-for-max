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

(define listen
  (lambda (inlet fun) 
    (post "adding listener on inlet " inlet) 
    (set! (s4m-listeners inlet) fun)))

;; dispatch is called from C, and expects a list where the first item
;; is the inlet number, and the rest are to be passed as arg list to listener
(define s4m-dispatch
  (lambda args
    ;;(post "s4m-dispatch :" args)
    ((s4m-listeners (car args)) (cdr args))))


;; wrapper for eval to help debugging or to hook into
;; called from C when a message is sent to max that we want treated as scheme code
;; this called with input to inlet 0 of the scm4max object
(define s4m-eval
  (lambda args
    (post "s4m-eval :" args)
    (eval args)))


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


