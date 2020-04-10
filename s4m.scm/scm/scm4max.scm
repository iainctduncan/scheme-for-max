;; the scm4max scheme code to build the API
;;(max-post "Bootstrapping scm4max.scm")

(define (stringify item)
  (cond
    ((symbol? item) (string-append "'" (symbol->string item)))
    ((number? item) (number->string item))
    ((string? item) item)
    ((procedure? item) "<procedure>")
    ;;((eq? #t) "#t")
    ;;((eq? #f) "#f") 
    (else "<unhandled type>"))) 

;; post arbitrary args to the max console
(define (post . args)
  (letrec (
    (log-string (lambda (lat)
                  (cond 
                    ((null? lat) "")
                    ((list? (car lat)) (string-append "<list: " (log-string (car lat)) (log-string (cdr lat)) ">")) 
                    (else (string-append (stringify (car lat)) " " (log-string (cdr lat))))))))
    (max-post (log-string args))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default callbacks that do nothing but remind you there's no callback
;; nicer than just "syntax-error" in the console
(define (f-bang) (post "Error: no f-bang function defined for bang message"))
(define (f-int arg) (post "Error: no f-int function defined for int messages"))
(define (f-float arg) (post "Error: no f-float function defined for float messages"))
(define f-list (lambda args (post "Error: no f-list function defined for list messages")))


;; listeners is a nested hash-table of inlet number and then listen keyword
(define s4m-listeners (make-hash-table))

;; which means when we add a listener, we need to make a hashtable if the key
;; is not there yet 

(define (listen inlet keyword fun)
   ;; (post "adding listener on inlet " inlet " with keyword " keyword) 
   ;; create nested hash at key {inlet-num} if not there already
   (if (not (s4m-listeners inlet)) 
       (set! (s4m-listeners inlet) (make-hash-table)))
   (set! ((s4m-listeners inlet) keyword) fun))

;; dispatch is called from C, and is passed a list of:
;; ({inlet} {keyword} .. args...)
(define s4m-dispatch
  (lambda args
    ;;(post "s4m-dispatch args:" args)
    (letrec* ( (inlet (car args))
               (keyword (cadr args))
               (func-args (cddr args)) 
               (listener ((s4m-listeners inlet) keyword)))
      (listener func-args))))


;; wrapper for eval to help debugging or to hook into
;; called from C when a message is sent to max that we want treated as scheme code
;; this called with input to inlet 0 of the scm4max object
(define s4m-eval
  (lambda args
    ;;(post "s4m-eval :" args)
    (eval args (rootlet))))


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

;;(post "scm4max.scm BOOTSTRAP COMPLETE")
