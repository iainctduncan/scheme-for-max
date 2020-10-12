(post "schedule.scm")


;; internal registry of callbacks registered by gensyms
(define s4m-callback-registry (hash-table))

(define (s4m-register-callback cb-function)
  (let ((key (gensym)))
    ;;(post "registering" cb-function "with key" key)
    (set! (s4m-callback-registry key) cb-function)
    key))


;; fetch a callback from the registry and delete it
;; it's a one-time fetch
(define (s4m-get-callback key)
  ;;(post "s4m-getcallback, key:" key)
  (let ((cb-function (s4m-callback-registry key)))
    (set! (s4m-callback-registry key) #f)    
    cb-function))


;; internal function to get a callback from the registry and run it
;; this gets called from C code when the C timing function happens
;; todo: later add env support
(define (s4m-execute-callback key)
  ;;(post "s4m-execute-callback" key)
  ;; get the func, note that this might return false if was cancelled
  (let ((cb-fun (s4m-get-callback key)))
    ;; dereg the handle
    (set! (s4m-callback-registry key) #f)
    ;; if callback retrieval got false, return false else execute function
    (if (eq? #f cb-fun) '() (cb-fun))))


; public function to delay a function by time ms (float)
; returns the callback key, which can be used to cancel it
(define (delay time arg)
  ;(post "(clock) time:" time "arg:" arg)
  ;; register the callback and return the handle
  (let ((cb-handle (s4m-register-callback arg)))
    ;; call the C ffi funtion and return the handle
    (s4m-schedule-delay time cb-handle)
    cb-handle))

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
;; delay 
(define (delay-eval time list-arg)
  (let ((cb-fun (lambda x (eval list-arg))))
    (delay time cb-fun)))   

;; alt sig to allow bundling for max messages
(define (delay-eval* time . list-arg)
  ;;(post "delay-eval* time: " time "args: " list-arg)
  (let ((cb-fun (lambda x (eval list-arg))))
    (delay time cb-fun)))   

(define (cancel key)
  ;;(post "de-registering callback for key" key)
  (set! (s4m-callback-registry key) #f))

