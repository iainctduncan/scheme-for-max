;(post "schedule.scm")

;; internal registry of callbacks registered by gensyms
(define s4m-callback-registry (hash-table))

(define (s4m-register-callback cb-function)
  (let ((key (gensym)))
    ;(post "registering" cb-function "with key" key)
    (set! (s4m-callback-registry key) cb-function)
    key))

;; fetch a callback from the registry 
(define (s4m-get-callback key)
  ;;(post "s4m-getcallback, key:" key)
  (let ((cb-function (s4m-callback-registry key)))
    cb-function))

;; internal function to get a callback from the registry and run it
;; this gets called from C code when the C timing function happens
;; todo: later add env support
(define (s4m-execute-callback key)
  ;(post "s4m-execute-callback" key)
  ;; get the func, note that this might return false if was cancelled
  (let ((cb-fun (s4m-get-callback key)))
    ;; dereg the handle
    (set! (s4m-callback-registry key) #f)
    ;; if callback retrieval got false, return false else execute function
    (if (eq? #f cb-fun) 
      '()
      ;; call our cb function, catching any errors here and posting
      (catch #t (lambda () (cb-fun)) (lambda err-args (post "ERROR:" err-args))))
))

; public function to delay a function by time ms (int or float)
; returns the gensym callback key, which can be used to cancel it
(define (delay time fun)
  ;(post "(delay) time:" time "args:" arg)
  ;; register the callback and return the handle
  (let ((cb-handle (s4m-register-callback fun)))
    ;; call the C ffi funtion and return the handle
    (s4m-schedule-delay time cb-handle)
    cb-handle))

;; delay with either a list or var args, 
;; i.e. (delay 1000 (list out 0 'bang)) or (delay-eval 1000 post :foo :bar)
(define (delay-eval time . args)
  ;;(post "delay-eval* time: " time "args: " list-arg)
  (let ((cb-fun (if (list? (car args)) 
                  (lambda x (eval (car args))) 
                  (lambda x (eval args)))))
    (delay time cb-fun)))    


; public function to delay a function by a tempo argument
; tempo aware version of delay, args can be numeric (ticks), or max notation ('4n etc)
(define (delay-t time fun)
  ;; register the callback, storing gensym handline in cb-handle 
  (let* ((cb-handle (s4m-register-callback fun)))
    (s4m-schedule-delay-t time cb-handle)
    cb-handle))

;; delay-t version of delay-eval
(define (delay-t-eval time . args)
  (let ((cb-fun (if (list? (car args)) 
                  (lambda x (eval (car args))) 
                  (lambda x (eval args)))))
    (delay-t time cb-fun))) 
 
; public function to delay a function by a tempo argument with a quant arg
; time and quant can be numeric (ticks), or max notation ('4n etc)
(define (delay-tq time quant fun)
  ;; register the callback, storing gensym handline in cb-handle 
  (let* ((cb-handle (s4m-register-callback fun)))
    (s4m-schedule-delay-tq time quant cb-handle)
    cb-handle))

;; delay with either a list or var args, 
;; i.e. (delay 1000 (list out 0 'bang)) or (delay-eval 1000 post :foo :bar)
(define (delay-tq-eval time quant . args)
  (let ((cb-fun (if (list? (car args)) 
                  (lambda x (eval (car args))) 
                  (lambda x (eval args)))))
    (delay-tq time quant cb-fun))) 

 
; old polymorphic version, deprecated for now
;(define (delay-tq . args)
;  ;(post "(delay-t) args:" args)
;  ;; register the callback, storing gensym handline in cb-handle 
;  (let* ((time (car args))
;        (quant (if (or (symbol? (args 1)) (number? (args 1))) (args 1) #f))
;        (fun (if quant (args 2) (args 1)))
;        (cb-handle (s4m-register-callback fun)))
;    ;; call the C ffi funtion and return the handle 
;    ;; quant will be #f if not passed in to this function
;    ;(post "time:" time "quant:" quant "handle:" cb-handle)
;    (if quant 
;      (s4m-schedule-delay-tq time quant cb-handle)
;      (s4m-schedule-delay-t time cb-handle))
;    cb-handle))


(define (cancel-delay key)
  ;;(post "de-registering callback for key" key)
  (set! (s4m-callback-registry key) #f))


;*******************************************************************************
; itm-listen-tick functions 


; global to hold the currently active tick callback (can only be one)
(define s4m-listen-ticks-callback #f)

;; internal function called from the C tick callback
(define (s4m-exec-listen-ticks-callback curr_ticks)
  ;(post "s4m-execute-tick-callback, tick:" curr_ticks)
  ;; as there can only be one of these at once, just use the global tick callback function
  (if (eq? #f s4m-listen-ticks-callback) '() (s4m-listen-ticks-callback curr_ticks)))

; public function to start listening every {ticks} ticks with function {fun}
; depends on transport running, expect tick to be integer
(define (clock-ticks ticks fun)
  (set! s4m-listen-ticks-callback fun)
  ;; call into C to register the listener 
  (s4m-itm-listen-ticks ticks)
  :clock-registered
) 

; public function to cancel the tick listener
(define (cancel-clock-ticks)
  ;(post "cancel-listen-ticks")
  ;; call into C to cancel the scheduled event
  (s4m-cancel-itm-listen-ticks)
  (set! s4m-listen-ticks-callback #f) 
)      

;*******************************************************************************
; itm-listen-ms functions - for transport aware ms listener 

; global to hold the currently active itm ms callback (can only be one)
(define s4m-itm-listen-ms-callback #f)

;; internal function called from the C ms callback
(define (s4m-exec-itm-listen-ms-callback)
  ;(post "s4m-exec-itm-listen-ms-callback time:" time-ms)
  ;; as there can only be one of these at once, just use the global tick callback function
  (if (eq? #f s4m-itm-listen-ms-callback) '() (s4m-itm-listen-ms-callback)))

; public function to start listening every {ms} miliseconds, but only if transport running 
(define (clock-ms-t ms fun)
  (set! s4m-itm-listen-ms-callback fun)
  ;; call into C to register the listener 
  (s4m-itm-listen-ms ms)
  :clock-registered
) 

; public function to cancel the itm ms listener
(define (cancel-clock-ms-t)
  ;(post "cancel-listen-ms-t")
  ;; call into C to cancel the scheduled event
  (s4m-cancel-itm-listen-ms)
  (set! s4m-itm-listen-ms #f) 
)

; Non-itm version of the above, runs regardless of transport state
; global to hold the currently active itm ms callback (can only be one)
(define s4m-listen-ms-callback #f)

;; internal function called from the C ms callback
(define (s4m-exec-listen-ms-callback)
  ;(post "s4m-exec-listen-ms-callback")
  ;; as there can only be one of these at once, just use the global tick callback function
  (if (eq? #f s4m-listen-ms-callback) '() (s4m-listen-ms-callback)))

; public function to start listenin every {ms} miliseconds
(define (clock-ms ms fun)
  (set! s4m-listen-ms-callback fun)
  ;; call into C to register the listener and return message
  (s4m-listen-ms ms) 
  :clock-registered) 

; public function to cancel the itm ms listener
(define (cancel-clock-ms)
  ;(post "(cancel-listen-ms)")
  ;; call into C to cancel the scheduled event
  (s4m-cancel-listen-ms)
  (set! s4m-listen-ms #f) 
)

;(post "... schedule.scm loaded")

