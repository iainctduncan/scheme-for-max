;; the scm4max scheme code to build the API
;(max-post "Bootstrapping s4m.scm")

;; stuff.scm contains the scheme level s7 helper definitions and must be loaded for various
;; s4m functions to work ok. it comes from S7 upstream 
(load-from-max "stuff.scm")

;; S74 is a convenience layer over S7, with functions from various other lisps
;; (Racket, Clojure, etc)
(load-from-max "s74.scm")

;; Uncomment the below to load the loop macro and various utilities from Common Music
;; They are not necessary for core Scheme-for-Max to run
;; NB: 2020-05-08 there is an issue with loop.scm on windows, we are working on it.
(load-from-max "loop.scm")
(load-from-max "utilities.scm")

;; misc convenience functions we use

;; catenated object to string reprs of things
(define (str-repr . args) 
 (let ((repr (lambda x (string-append (object->string x) " "))))
   (apply string-append (map repr args))))

;; convert to a string for our post function
(define (stringify item)
  (cond
    ((keyword? item) (symbol->string item))
    ((symbol? item) (string-append "'" (symbol->string item)))
    ((number? item) (number->string item))
    ((string? item) item)
    ((procedure? item) "<procedure>")
    ((and (boolean? item) (eq? item #t)) "#true")
    ((and (boolean? item) (eq? item #f)) "#false")
    (else (object->string item)))) 

;; post arbitrary args to the max console
;; allows calling like (post "my thing" thing "other thing" other-thing)
(define (post . args)
  (letrec (
    (log-string (lambda (lat)
                  (cond 
                    ((null? lat) "")
                    (else (string-append (stringify (car lat)) " " (log-string (cdr lat))))))))
    (max-post (log-string args))
))

;; helper to set whether we see nulls logged to the console, defaults to false
(define s4m-log-nulls #t)
(define (s4m-filter-result res)
  ;; if we replace what would be returned by :no-log, s4m will not print to console
  (cond 
    ;; turn off loging of the null list if set to do so
    ((and (null? res) (not s4m-log-nulls)) :no-log)
    ;; use the same setting to mute logging lists of nulls: (() () ())
    ((and (list? res) (every? null? res) (not s4m-log-nulls)) :no-log)
    (else res))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default callbacks that do nothing but remind you there's no callback registered yet
;; They are just more helpful than "syntax-error" in the console
(define (f-bang) (post "Error: no f-bang function defined for bang message"))
(define (f-int arg) (post "Error: no f-int function defined for int messages"))
(define (f-float arg) (post "Error: no f-float function defined for float messages"))
(define f-list (lambda args (post "Error: no f-list function defined for list messages")))


;; the listeners registry is a nested hash-table of inlet number and listen keyword/symbol 
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
    (letrec* ( (inlet (car args))
               (keyword (cadr args))
               (func-args (cddr args)) 
               (listener (cond 
                  ((s4m-listeners inlet) ((s4m-listeners inlet) keyword))
                  (else #f))))   
      (if (procedure? listener) 
            (listener func-args)
            (post "Error: no listener on inlet" inlet "for" keyword)))))


;; The function used when we send code to inlet 0 that we want evaluated
;; called from C when a message is sent to max that we want treated as scheme code
;; we add rootlet as the environment so that definitions created that way are visible
;; to all other code
(define s4m-eval
  (lambda args
    ;(post "s4m-eval :" args)
    (eval args (rootlet))))


;; roll all this in later
(load-from-max "schedule.scm")

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

(define s4m-done-bootstrap #t)

;(post "s4m.scm BOOTSTRAP COMPLETE")
