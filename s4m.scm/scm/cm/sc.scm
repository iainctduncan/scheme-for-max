;;; **********************************************************************
;;; Copyright (c) 2010 Michael Klingbeil, Rick Taube.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

(define* (make-sc-env env (curve 0))
  ;; convert a CLM style envelope to a SuperCollider style envelope
  ;; specify a curve value 0 = linear slopes
  ;; negative values = convex for increasing slopes, concave for decreasing
  ;; positive values = concave for increasing slopes, convex for decreasing
  ;; can be a list of values specifying a curve for each segment
  (let* ((xmax (let recur ((items env))
                 (if (null? (cddr items))
                     (exact->inexact (car items))
                     (recur (cdr items)))))
         (head (list (cadr env)  ; initial level
                     (- (floor (/ (length env) 2)) 1) ; num nodes
                     -99   ; release node magic num
                     -99)) ; loop node magic num
         (tail (cdddr head)))
    ;; build sc envelope in place without copying
    (do ((data (cddr env) (cddr data))
         (last 0)
         (cval #f))
        ((null? data)
         head)
      (if (pair? curve)
          (begin (set! cval (car curve)) (set! curve (cdr curve)))
          (set! cval curve)) 
      (set-cdr! tail (list (cadr data) ; level
                           (/ (- (car data) last) xmax) ; wait
                           5 ; magic num?
                           cval))
      (set! tail (cddddr tail))
      (set! last (car data)))))

; (make-sc-env '(0 0 50 1 100 0) 33)
; (make-sc-env '(0 1 100 0) 33)
; (make-sc-env '(0 1 10 1 50 1 100 0) -4)
; (make-sc-env '(0 0 10 1 100 1) )

(define* (sc:hush (group 1))
  ;; free all sounding nodes in group
  (osc:message "/g_freeAll" group))

(define (sc:alloc-buffers . dbs)
  ;; loads sc sound buffers for the specified sound dbs.  first
  ;; argument can be a starting buffer number, defaults to
  ;; 0. following that comes optionally tagged sound dbs, where tags
  ;; are :mono :stereo or #t (both). tags are sticky and default to
  ;; #t. returns the number of buffers allocated
  (if (not (osc:open? ))
      (error "OSC port not open, can't allocate buffers."))
  (do ((bufnum (if (number? (car dbs)) (car dbs) 0 ))
       (tail (if (number? (car dbs)) (cdr dbs) dbs) (cdr tail))
       (mode #t))
      ((null? tail) bufnum)
    (cond ((member (car tail) '(#t :both :mono :stereo))
           (set! mode (car tail)) )
          ((vector? (car tail))
           (do ((i 0 (+ i 1))
                (db (car tail))
                (len (length (car tail)))
                (entry #f)
                (file #f))
               ((not (< i len)) #f)
             (set! entry (vector-ref db i))
             (set! file (cadr entry))
             (cond ((eq? mode :stereo)
                    (osc:message "/b_allocRead" bufnum file)
                    (set-car! (cddr entry) bufnum)
                    (set! bufnum (+ bufnum 1))
                    )
                   ((eq? mode :mono)
                    (osc:message "/b_allocReadChannel" bufnum file 0 0 0)
                    (set-car! (cddr entry) bufnum)
                    (set! bufnum (+ bufnum 1))
                    )
                   (else
                    ;; allocate both :stereo and :mono
                    (osc:message "/b_allocRead" bufnum file)
                    (osc:message "/b_allocReadChannel" (+ bufnum 1) file 0 0 0)
                    (set-car! (cddr entry) (list bufnum (+ bufnum 1)))
                    (set! bufnum (+ bufnum 2))
                    ))))
          (else
           (error "sc:alloc-buffers: invalid input ~S" (car tail)))
          )))

;; load sound files into SuperCollider buffers
(define (load-sc-buffers buffer-start-index db)
  (let recur ((i 0))
    (if (= i (vector-length db))
      #t
      (let ((bufnum (+ i buffer-start-index)))
        (osc:message "/b_allocRead" bufnum (cadr (vector-ref db i)))
        (set-car! (cddr (vector-ref db i)) bufnum)
        (recur (1+ i))))))

;; load a single channel of sound files into mono SuperCollider buffers
(define (load-sc-buffers-mono buffer-start-index db)
  (let recur ((i 0))
    (if (= i (vector-length db))
      #t
      (let ((bufnum (+ i buffer-start-index)))
        (osc:message "/b_allocReadChannel" bufnum (cadr (vector-ref db i)) 0 0 0)
        (set-car! (cddr (vector-ref db i)) bufnum)
        (recur (1+ i))))))


