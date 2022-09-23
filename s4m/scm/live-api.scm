(post "loading live-api.scm")

; Scheme for Max Live-API interface
; by Iain Duncan, September 2022

; Note: this will only run in an s4m @thread low instance, it will refuse to do anything from high thread
; As all live.api objects defer anyway, this makes no difference.
; If you want to use the Live API from an s4m @thread high instance, make a low instance and send messages
; from your high instance to the low instance, which will be automatically defered.
;
; This code assumes s4m-live-api subpatcher exists and is connected to your s4m instance.
; You can make this automatically by sending s4m the init-live-api message, or calling (build-live-api) from your code.
; However, this does not (yet) check for and remove duplicate s4m-live-api subpatcher objects!

; The main interface to the live-api is the send-path method
; Below are some sample high level functions you might make that use the live-api 
; object's send-path method, which expects two arguments:
; - a list of the API LOM path
; - a list of the message you want to send the object

(define (live-play)
  (live-api 'send-path '(live_set) '(set is_playing 1)))

(define (live-stop)
  (live-api 'send-path '(live_set) '(set is_playing 0)))

(define (fire-clip track slot)
  (live-api 'send-path (list 'live_set 'tracks track 'clip_slots slot 'clip) 
    '(call fire)))
  
(define (stop-clip track slot)
  ; as above, but using back-tick lisp syntax to inject arg values for track and slot
  (live-api 'send-path `(live_set tracks ,track clip_slots ,slot clip) 
    '(call stop)))

(define (fire-scene scene)
  (live-api 'send-path `(live_set scenes ,scene) '(call fire)))

(define (stop-scene scene)
  (live-api 'send-path `(live_set scenes ,scene) '(call stop)))

(define (set-device-parameter track device param value)
  (live-api 'send-path `(live_set tracks ,track devices ,device parameters ,param) 
     `(set value ,value)))

(define (get-device-parameter track device param)
  (live-api 'send-path `(live_set tracks ,track devices ,device parameters ,param) 
     `(get value)))


;********************************************************************************
; The low-level live-api object, normally you should not need to change this
; called with (live-api 'send-path (..path) (..msg))
(define live-api
  (let ((obj-id #f)   ; object id of last path request
        (value #f)    ; value received for successful value requests
        (debug #f))   ; set to true to see debug messages in the console

    ; send a live message to a live API path, main public method
    ; first find-path fins the object and updates the obj-id instance var
    ; then send-object sends the object a message
    ; if this resulted in value being update, this is returned
    (define (send-path path msg)
      (log-debug "live-api.send-path" path msg)
      (find-path path)
      (send-object msg))
 
    ; method to find an object from a live path
    ; results in live.path object calling back with the update-id callback
    (define (find-path path)
      (log-debug "(live-api.find" path ")")
      (set! obj-id #f)
      ; send the live.path object a path message, it will trigger the 'id message
      (apply send (cons 's4m-live-path (cons 'path path))
      ))

    ; the id callback, called from the response from the live.path obj
    ; updates internal (last object) id and sends id message to the live.object
    (define (update-id id)
      (log-debug "live-api.update-id" id)
      (set! obj-id id)
      (send 's4m-live-object 'id obj-id))

    ; the value callback, called from the response from the live.object obj
    ; updates internal (last object) value so caller can return value
    (define (update-value value-in)
      (log-debug "live-api.update-value" value-in)
      (set! value value-in))

    (define (send-object msg-list)
      (log-debug "live-api.send-object" msg-list )
      (set! value #f)
      (apply send (cons 's4m-live-object msg-list))
      ; if the above resulted in an update to value, return it, else null
      (if value value '()))
   
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


; function to build the live api subpatch, can be called from user source code
(define (make-live-api)
  ; use a delay here to ensure everything has settled
  (delay 0 s4m-init-live-api))


