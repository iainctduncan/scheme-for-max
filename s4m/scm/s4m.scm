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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From here down, you should not change things unless you know what it's going to do

; function for getting attributes from the s4m object
; this is a function so that users won't try to set on the attr hash from scheme
(define (*s4m* key)
  (_s4m_ key))

(define (send . args)
  (apply s4m-send args))

; s4m-send expects flat args, flatten args
(define (send* . args)
  ;(post "send" args)
  (apply s4m-send
    (let myloop ((new-args '()) (left args))
      ;(post "loop" new-args left)
      (cond 
        ((null? left)
          new-args)
        ((sequence? (car left))
          ;(post "seq, calling loop" (append new-args (car left)) (cdr left))
          (myloop (append new-args (car left)) (cdr left)))
        (else
          ;(post "not seq, calling loop" (append new-args (list (car left))) (cdr left))
          (myloop (append new-args (list (car left))) (cdr left)))
      ))))


;; gc functions
;; wrapper for debugging only

;; a wrapper that you can interfere with
;; called from gc-try and gc-run, runs gc
(define (s4m-gc)
  ;;(post "(s4m-gc) running (gc)")
  ;; gc both runs and enables the gc, if this is called from gc-run or gc-try
  ;; the gc will get set back to where it should be in the C code
  (gc))


;; helper for building string reps for posting to console
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
    ;; turn off logging of the null list if set to do so
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


; temp code for s4m-expr, will get moved to the c handler
; we only need one for each inlet in the s4m-expr (the inX)
;(define in1 #f)
;(define in2 #f)
;(define in3 #f)
;(define in4 #f)
;(define in5 #f)
;(define in6 #f)
;(define in6 #f)
;(define in7 #f)


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
(define (out outlet_num val) 
  (max-output outlet_num val))

; out* - special output that spreads sequences over the available outlets
(define (out* val)
  ;(post "out*" val)
  (let ((outs (*s4m* :outs))
        (num-vals (length val)))
    (cond
      ((not (sequence? val))
        (out 0 val))
      ((sequence? val)
        ; loop through outs, if we're on the last out, send out rest of list
        (let out-loop ((outlet 0) (left val))
          (cond
            ((null? left) '())
            ((and (< outlet (- outs 1)) (not-null? val))
              (out outlet (car left))  
              (out-loop (inc outlet) (cdr left)))
            (else 
              (out outlet left)))))))
  ; in all cases, out should return null
  '()) 

(define (out-0 args) (max-output 0 args))
(define (out-1 args) (max-output 1 args))
(define (out-2 args) (max-output 2 args))
(define (out-3 args) (max-output 3 args))
(define (out-4 args) (max-output 4 args))
(define (out-5 args) (max-output 5 args))
(define (out-6 args) (max-output 6 args))
(define (out-7 args) (max-output 7 args))

(define s4m-done-bootstrap #t)

;(post "s4m.scm init complete")
