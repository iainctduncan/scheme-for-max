(post "schedule.scm")


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
  (post "s4m-execute-callback" key)
  ;; get the func, note that this might return false if was cancelled
  (let ((cb-fun (s4m-get-callback key)))
    ;; dereg the handle
    (set! (s4m-callback-registry key) #f)
    ;; if callback retrieval got false, return false else execute function
    (if (eq? #f cb-fun) '() (cb-fun))))


; public function to delay a function by time ms (float)
; returns the callback key, which can be used to cancel it
(define (delay time arg)
  (post "(delay) time:" time "arg:" arg)
  ;; register the callback and return the handle
  (let ((cb-handle (s4m-register-callback arg)))
    ;; call the C ffi funtion and return the handle
    (s4m-schedule-delay time cb-handle)
    cb-handle))

; public function to delay a function by a tempo argument
; tempo aware version of delay, args can be numeric (ticks), or max notation ('4n '1:1:1 etc)
(define (delay-t . args)
  ;(post "(delay-t) args:" args)
  ;; register the callback, storing gensym handline in cb-handle 
  (let* ((time (car args))
        (quant (if (or (symbol? (args 1)) (number? (args 1))) (args 1) #f))
        (fun (if quant (args 2) (args 1)))
        (cb-handle (s4m-register-callback fun)))
    ;; call the C ffi funtion and return the handle 
    ;; quant will be #f if not passed in to this function
    ;(post "time:" time "quant:" quant "handle:" cb-handle)
    (s4m-schedule-delay-itm time quant cb-handle)
    cb-handle))

;; delay evaling a list by time ms
(define (delay-eval time list-arg)
  (let ((cb-fun (lambda x (eval list-arg))))
    (delay time cb-fun)))   

;; alternate version to allow calling with an inline list of args 
(define (delay-eval* time . list-arg)
  ;;(post "delay-eval* time: " time "args: " list-arg)
  (let ((cb-fun (lambda x (eval list-arg))))
    (delay time cb-fun)))   

;; TODO tempo aware versions of the above

(define (cancel key)
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
(define (listen-ticks ticks fun)
  (set! s4m-listen-ticks-callback fun)
  ;; call into C to register the listener 
  (s4m-itm-listen-ticks ticks)) 

; public function to cancel the tick listener
(define (cancel-listen-ticks)
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
(define (s4m-exec-itm-listen-ms-callback time-ms)
  ;(post "s4m-exec-itm-listen-ms-callback time:" time-ms)
  ;; as there can only be one of these at once, just use the global tick callback function
  (if (eq? #f s4m-itm-listen-ms-callback) '() (s4m-itm-listen-ms-callback time-ms)))

; public function to start listening every {ms} miliseconds, but only if itm running 
(define (itm-listen-ms ms fun)
  (set! s4m-itm-listen-ms-callback fun)
  ;; call into C to register the listener 
  (s4m-itm-listen-ms ms)) 

; public function to cancel the itm ms listener
(define (cancel-itm-listen-ms)
  (post "cancel-itm-listen-ms")
  ;; call into C to cancel the scheduled event
  (s4m-cancel-itm-listen-ms)
  (set! s4m-itm-listen-ms #f) 
)

