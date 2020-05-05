;;; **********************************************************************
;;; Copyright (C) 2008-2010 Rick Taube.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

;; loading this file ensures implementation features that are either
;; not provided or unevenly provided in scheme implementations. see
;; readme.txt for the list of features.

(define *error-trace* #t)

(define (s7-error-hook tag args)
  ;; report scheme errors to Grace's console window in the same format
  ;; as C-side errors.
  (let ((port (open-output-string)))
    (format port ">>> Error: ")
    ;; if the error is triggered by s7 then args can be a string,
    ;; otherwise when cm triggers an error args is always a list of
    ;; args, if more than one arg then the first is a format string
    ;; and the others are args for format
    (if (pair? args)
        (if (null? (cdr args)) ;; a constant string error message
            (display (car args) port)
            (apply format port args)) ;; a format string plus args
        (display args port))
;    (format port "~%")
    (if *error-trace* 
        (format port "~A~%" (stacktrace)))
  (let ((str (get-output-string port)))
    (close-output-port port)
    (ffi_print_error str))
  ;; +s7-error+ is a C-defined constant that Scheme returns to signal
  ;; the C side that an error has occured during evaluation
  +s7-error+
  ))

;;(set! *error-hook* s7-error-hook)  ; s7's error hook variable 

(set! (hook-functions *error-hook*)
      (list (lambda (hook)
              (set! (hook 'result) ;(s7-error-hook (hook 'type) (hook 'data))
                    (let ((tag (hook 'type))
                          (args (hook 'data))
                          (port (open-output-string)))
                      (format port ">>> Error: ")
                      ;; if the error was triggered by an s7 primitive
                      ;; then args can be a string, otherwise when cm
                      ;; triggers an error args is always a list of
                      ;; args, if more than one arg then the first is
                      ;; a format string and the others are args for
                      ;; format
                      (if (pair? args)
                          (if (null? (cdr args)) ; a constant string error message
                              (display (car args) port)
                              (apply format port args)) ; a format string plus args
                          (display args port))
                      (if *error-trace* 
                          (let ((str (stacktrace)))
                            (format port "~%~A~%" str)
                            ;(format port "~%~A~%" (substring str (char-position #\newline str)) )
                            ))
                      (let ((str (get-output-string port)))
                        (close-output-port port)
                        (ffi_print_error str))
                      ;; +s7-error+ is a C-defined constant that
                      ;; Scheme returns to signal the C side that an
                      ;; error has occured during evaluation
                      +s7-error+
                      )))))


;; a version of (load file) that will load instrument files from the
;; embedded sources if (1) the file has no directory component, (2)
;; the file does not exist in the current working directory and (3)
;; the *load-path* variable is null

(define s7-load load)
(define load ffi_load)

(define (interaction-environment ) (global-environment))

(define-macro (define-record . args) (values)) ; records in s7Foreign

;;(define-macro (unless arg . body) 
;;  `(if (not ,arg) (begin ,@ body)))
;;
;;(define-macro (when arg . body) 
;;  `(if ,arg (begin ,@ body)))

;; keywords

(define (string->keyword s)
  (make-keyword s))

(define (keyword->string k)
  (symbol->string (keyword->symbol k)))

(define fixnum? integer?)

(define (logtest a b)
  (not (zero? (logand a b))))

;; hash tables

(define (make-equal-hash-table)
  (make-hash-table))

(define (make-eq-hash-table)
  (make-hash-table))

(define hash-set! hash-table-set!)

(define hash-ref hash-table-ref)

;; strings

(define (read-from-string str)
  (call-with-input-string str read))

;; bill sez: "sort! copies lists, but does change vectors (it's really vector-sort!
;; with list->vector and vector->list if it gets a list argument"
;; adapted for lists as well as vectors from
;; http://www.math.grin.edu/~stone/events/scheme-workshop/quicksort.html

(define sort sort!)

;; destructive sort for both lists and vectors.
;; http://www.math.grin.edu/~stone/events/scheme-workshop/quicksort.html

(define (qsort! vec . opt)
  (let* ((precedes? (if (null? opt) < (car opt)))
         (getter #f)
         (swapper #f)
         (sizeof #f))
    (cond ((vector? vec)
           (set! getter vector-ref)
           (set! swapper (lambda (s i j)
                           (let ((x (vector-ref s i)))
                             (vector-set! s i (vector-ref s j))
                             (vector-set! s j x))))
           (set! sizeof vector-length)
           )
          ((pair? vec)
           (set! getter list-ref)
           (set! swapper (lambda (s i j)
                           (let ((x (list-ref s i)))
                             (list-set! s i (list-ref s j))
                             (list-set! s j x))))
           (set! sizeof length))
          (else
           (error "qsort!: argument not a vector or list: ~S" vec)))
    (let* ((partition!
            (lambda (start stop pivot)
              (letrec ((rightwards
                        (lambda (current)
                          (if (and (< current stop)
                                   (precedes? (getter vec current)
                                              pivot))
                              (rightwards (+ current 1))
                              current)))
                       (leftwards
                        (lambda (current)
                          (if (or (< current start)
                                  (precedes? (getter vec current)
                                             pivot))
                              current
                              (leftwards (- current 1))))))

                (let lupe ((left-pointer (rightwards start))
                           (right-pointer (leftwards (- stop 1))))
                  (if (< left-pointer right-pointer)
                      (begin
                        (swapper vec left-pointer right-pointer)
                        (lupe (rightwards (+ left-pointer 1))
                              (leftwards (- right-pointer 1))))
                      left-pointer))))))

      (let qs ((start 0)
               (stop (- (sizeof vec) 1)))
        (if (< start stop)
            (let* ((pivot (getter vec stop))
                   (break (partition! start stop pivot)))
              (swapper vec break stop)
              (if (<= (- break start)
                      (- stop break))
                  (begin
                    (qs start (- break 1))
                    (qs (+ break 1) stop))
                  (begin
                    (qs (+ break 1) stop)
                    (qs start (- break 1)))))))
      vec)))

