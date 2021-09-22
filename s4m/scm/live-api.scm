;(post "live-api.scm")

; Scheme for Max Live-API interface
; by Iain Duncan, September 2021

; Note: this will only run in an s4m @thread low instance, it will refuse to do anything from high thread
; As all live.api objects defer anyway, this makes no difference.
; If you want to call it from an s4m @thread high instance, make a low instance and send it messages,
; which get automatically defered.
;
; This code assumes the following patching, where {} indicates the scripting name of the object:
; |live.path {live-path}| -> |route id| -> |prepend live-api 'id| -> |s4m @thread low|
; |id $1 {live-object-id}| -> |live.object {live-object}| -> |route value| -> |prepend live-api 'value| -> |s4m @thread low|


;********************************************************************************
; Below are some sample high level functions you might make that use the live-api 
; object's send-path method, which expects two arguments:
; - a list of the API LOM path
; - a list of the message you want to send the object

(define (fire-clip track slot)
  (live-api 'send-path (list 'live_set 'tracks track 'clip_slots slot 'clip) 
  (list 'call 'fire)))
  
(define (stop-clip track slot)
  ; as above, but using back-tick lisp syntax
  (live-api 'send-path `(live_set tracks ,track clip_slots ,slot clip) 
  '(call stop)))

(define (play)
  (live-api 'send-path `(live_set) '(set is_playing 1)))

(define (stop)
  (live-api 'send-path `(live_set) '(set is_playing 0)))

;********************************************************************************
; The low-level api, normally you should not need to change this
(define live-api
  (let ((obj-id #f)   ; object id of last path request
        (value #f)    ; value received for successful value requests
        (debug #f))   ; set to true to see messages in the console
 
    ; the id callback, called from the response from the live.path obj
    ; updates internal (last object) id and sends id message to the live.object
    (define (update-id id)
      (log-debug "live-api.update-id" id)
      (set! obj-id id)
      (send 'live-object-id 'int obj-id))

    ; the value callback, called from the response from the live.object obj
    ; updates internal (last object) value so caller can return value
    (define (update-value value-in)
      (log-debug "live-api.update-value" value-in)
      (set! value value-in))

    ; method to find an object from a live path
    ; results in live.path object calling back with the id callback
    (define (find-path path)
      (log-debug "(live-api.find" path ")")
      (set! obj-id #f)
      (apply send (cons 'live-path (cons 'path path))))

    (define (send-object msg-list)
      (log-debug "live-api.send-object" msg-list )
      (set! value #f)
      (apply send (cons 'live-object msg-list))
      ; if the above resulted in an update to value, return it, else null
      (if value value '()))

    (define (send-path path msg)
      (log-debug "live-api.send-path" path msg)
      (find-path path)
      (send-object msg))

    (define (log-debug . args)
      (if debug (apply post args) '()))

    ; dispatcher
    (lambda (msg . args)
      (if (isr?)
        (post "Error: live-api requires s4m @thread low")
        (case msg
          ('id        (update-id (args 0)))     
          ('value     (update-value (args 0)))     
          ('path      (find-path (args 0)))
          ('object    (send-object (args 0)))
          ('send-path (send-path (args 0) (args 1)))
          (else '()))))
))    

