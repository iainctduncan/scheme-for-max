;;; **********************************************************************
;;; Copyright (C) 2009, Rick Taube
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

(define (osc:open port target)
  (define (checkport port checktarg)
    (cond ((integer? port)
           (if (not (> port 0))
               (error "invalid port number: ~A" port))
           (number->string port))
          ((string? port)
           (if (and checktarg (position #\: port))
               (let ((p (position #\: port)))
                 (unless (and (> p 0) (< p (- (length port) 1)))
                   (error "invalid host:port string: ~S" port))
                 (checkport (substring port (+ p 1)) #f)
                 target)
               (let ((p (string->number port)))
                 (if (or (not p)
                         (not (integer? p))
                         (not (> p 0)))
                     (error "invalid port number: ~A" port))
                 port)))
          (else
           (error "invalid port number: ~S" port))))
  (ffi_osc_open (checkport port #f) (checkport target #t)))

; (osc:open 100 "goo:900")
; (osc:open 100 "goo:900")
; (osc:open 100 ":900")
; (osc:open 100 "aaa:")

(define (osc:open? )
  (ffi_osc_open_p))
 
(define (osc:message path . data)
  (if (null? data)
      (if (string? path)
          (ffi_osc_send_message path data)
          (if (pair? path)
              (if (string? (car path))
                  (ffi_osc_send_message (car path) (cdr path))
                  (error "not an OSC message: ~S" path))
              (error "not an OSC message: ~S" path)))
      (if (string? path)
          (ffi_osc_send_message path data)
          (error "not an OSC path: ~S" path))
      ))

; (osc:message "/hi" 1 2)
; (osc:message '("/hi" 1 2))

; (osc:bundle 100 '("/hi" 1) '("/ho" 2))
; (osc:bundle 100 '(("/hi" 1) ("/ho" 2)))
; (osc:bundle '(100 ("/hi" 1) ("/ho" 2)))
; (osc:bundle '(100 (("/hi" 1) ("/ho" 2))))

;data ( ("/hi" 1) ("/ho" 2)))
;data ( (("/hi" 1) ("/ho" 2))))
;(OSC:BUNDLE 100 (("/hi" 1) ("/ho" 2)))

(define (osc:bundle time . data)
  (if (null? data)
      (if (pair? time)
          (begin (set! data (cdr time))
                 (set! time (car time)))
          (error "not an OSC bundle: ~S" time)))
  (if (number? time)
      (if (pair? (car data)) ; data must be a message or list of messages
          (if (pair? (caar data)) ; messages passed as single list
              (if (null? (cdr data))
                  (ffi_osc_send_bundle time (car data))
                  (error "not an OSC bundle: ~S" (car data)))
              (ffi_osc_send_bundle time data))
          (error "not an OSC message: ~S" (car data)))
      (error "not an OSC time tag: ~S" time)))

(define (osc:close)
  (ffi_osc_close))

(define (osc:receive . args)
  (if (null? args) ; clear all hooks
      (ffi_osc_set_hook "*" #f)
      (let ((arg (car args))
            (rest (cdr args)))
        (if (null? rest) ; set/clear default hook
            (if (not arg) 
                (ffi_osc_set_hook "" #f) ; clear default hook     
                (if (and (procedure? arg)
                         ;(= (car (procedure-arity arg) ) 1)
                         )
                    (ffi_osc_set_hook "" arg)
                    (error "osc:receive: receiver not #f or a procedure of one argument: ~S"
                           arg)))
            ;; rest is ( proc|#f)
            (let ((op arg)
                  (proc (car rest))
                  (rest (cdr rest)))
              (if (null? rest)
                  (if (string? op)
                      (if (or (not proc) ;; clear/set valid hook
                              (and (procedure? proc)
                                   ;(= (car (procedure-arity proc)) 1)
                                   ))
                          (ffi_osc_set_hook op proc)
                          (error "osc:receive: receiver not #f or a procedure of one argument: ~S"
                                 proc))
                      (error "osc:receive: invalid path: ~S" op))
                  (error "osc:receive: too many arguments: ~S" args)))))))

(define (osc:receive? . args)
  (if (null? args)
      (ffi_osc_is_hook "*") ; is any hook set?
      (if (null? (cdr args))
          (let ((path (car args)))
            (if (string? path)
                (ffi_osc_is_hook path)
                (error "osc:receive?: invalid path: ~S" op)))
          (error "osc:receive?: too many arguments: ~S" args))))

