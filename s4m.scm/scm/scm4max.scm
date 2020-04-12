;; the scm4max scheme code to build the API
;;(max-post "Bootstrapping scm4max.scm")

;; By default, we load the following s7 extras below
;; the are not necessary for core Scheme-for-Max to run though, so you can disable
;; if you know you don't want them
(load-from-max "stuff.scm")

;; can't load r7rs.scm without loading libc.scm, and that's throwing an error
;;(load-from-max "libc.scm")
;;(load-from-max "r7rs.scm")

;; convert to a string for our post function
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
;; allows calling like (post "my thing" thing "other thing" other-thing)
(define (post . args)
  (letrec (
    (log-string (lambda (lat)
                  (cond 
                    ((null? lat) "")
                    ((list? (car lat)) (string-append "<list: " (log-string (car lat)) (log-string (cdr lat)) ">")) 
                    (else (string-append (stringify (car lat)) " " (log-string (cdr lat))))))))
    (max-post (log-string args))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default callbacks that do nothing but remind you there's no callback registered yet
;; They are just more helpful than "syntax-error" in the console
(define (f-bang) (post "Error: no f-bang function defined for bang message"))
(define (f-int arg) (post "Error: no f-int function defined for int messages"))
(define (f-float arg) (post "Error: no f-float function defined for float messages"))
(define f-list (lambda args (post "Error: no f-list function defined for list messages")))


;; the listeners registrry is a nested hash-table of inlet number and then listen keyword
(define s4m-listeners (make-hash-table))

;; the listen function, used to register listeners for inlet > 0
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
      (if (procedure? listener) 
            (listener func-args)
            (post "Error: no listener on " inlet keyword)))))


;; The function used when we send code to inlet 0 that we want evaluated
;; called from C when a message is sent to max that we want treated as scheme code
;; we add rootlet as the environment so that definitions created that way are visible
;; to all other code
(define s4m-eval
  (lambda args
    ;;(post "s4m-eval :" args)
    (eval args (rootlet))))


;; convenience functions for output, sometimes you want a one arg function...
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
(define s4m-done-bootstrap #t)
