;;; **********************************************************************
;;; Copyright (C) 2008, 2009 Rick Taube.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

;; *process-stop* is the code that a process executes to return the
;; 'stop value' back to C to signals no more scheduling. -1 is a
;; normal exit, -2 is an error exit.  the call to (throw ) has to be
;; implemented in the scheme we are running in. in chicken scheme it
;; is a call/cc exit, in s7 it is a call to (error )

;(define *process-stop* '(throw (quote all-done)))
(define *process-stop* '(return -1))
(define (run-while-until forms clauses ops)
  (let ((head forms)
        (oper (pop forms))
        (test #f)
        (stop *process-stop*))
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (case oper
      ((until) (set! test (pop forms)))
      ((while) (set! test `(not ,(pop forms)))))
    (values (make-loop-clause 'operator oper 'looping
             (list `(if ,test ,stop)))
            forms)))

(define *run-operators*
  (let* ((omit '(collect append nconc sum count
			 minimize maximize thereis
			 always never return
			 while until))
	 (head (list #f))
	 (tail head))
    (do ((ops *loop-operators* (cdr ops)))
	((null? ops)
	 (set-cdr! 
	  tail (list (list 'while (function run-while-until) #f )
		     (list 'until (function run-while-until) #f )))
	 (cdr head))
      (unless (member (car (car ops)) omit)
	(set-cdr! tail (list (car ops)))
	(set! tail (cdr tail))))))

;; (pprint  *run-operators*)

(define (process-code-terminates? code stop)
  ;; see if the stop form is anywhere in the expansions, if not issue
  ;; a warning.  FIX: this should be a call/cc
  (if (null? code) #f
      (if (pair? code)
	(or (process-code-terminates? (car code) stop)
	    (process-code-terminates? (cdr code) stop))
	(equal? code (car stop)))))

(define (expand-process forms )
  ;; (format #t "in expand-process~%") 
  ;; if called by sal forms is the vector already parsed by
  ;; make-loop-clause
  (let* ((parsed (if (vector? forms) 
		     forms
		     (parse-iteration 'process forms *run-operators*)))
	 (code '())
	 (func #f)
	 (tests '())
	 ;; *process-stop* is form that gets executed to stop
	 ;; the process
	 (done *process-stop*)
	 (TIME (gensym "time"))
	 (WAIT (gensym "wait"))
	)
    ;;(format #t "parsed=~S~%" (vector->list parsed))
    (set! tests (loop-end-tests parsed))
    (if (not (null? (loop-finally parsed)))
      (set! done `(begin ,@(loop-finally parsed) ,done)))
    (if (not (null? tests))
      (begin
       (if (null? (cdr tests))
	 (set! tests (car tests))
	 (set! tests (cons 'or tests)))
       (set! tests `((if ,tests ,done))))
      (unless (process-code-terminates?
	       (loop-looping parsed) done)
	(print-output "Warning: possible non-terminating process.")
        ))
    (set! func 
	  `(lambda (,TIME )
	     (let* ((,WAIT 0)
		    (elapsed (lambda args
			       (if (null? args)
				   ,TIME
				   (if (and (car args)
					    (ffi_sched_score_mode_p))
				       (ffi_sched_score_time)
				       ,TIME))))
		    (wait (lambda (x) (set! ,WAIT x))))
               (call-with-exit
                (lambda (return)
                  ,@ (append tests (loop-looping parsed)
                             (loop-stepping parsed))
                     ,WAIT))
;	       (catch 'all-done 
;                      (lambda () 
;                        ,@ (append tests (loop-looping parsed)
;                                   (loop-stepping parsed) ;;(list WAIT)
;                                   ))
;                      (lambda catchargs (set! , WAIT -1)))
;               , WAIT
	       )))
    (if (and (null? (loop-bindings parsed))
	     (null? (loop-initially parsed)))
	func
	;; use let* sequential binding
	`(let* ,(loop-bindings parsed)
	   ,@(loop-initially parsed)
	   ,func))))

(define-expansion  (process . args)
  (expand-process args ))

(define-macro (define-process formals . body)
  (unless (and (pair? formals)(symbol? (car formals)))
    (error "illegal process formals ~S" formals))
  `(define ,formals ,@ body))


; (define foo (process for i below 3 do (display i) ))
; (foo 0)

;;;
;;; scheduler api
;;;

; (sprout aaa 0 "test.snd" ...)

(define *sprout-hook* #f)

(define (sprout-hook . hook)
  (if (not (null? hook))
      (if (not (car hook))
          (set! *sprout-hook* #f)
          (if (procedure? (car hook))
              (if (= (car (procedure-arity (car hook))) 1)
                  (set! *sprout-hook* (car hook))
                  (error "sprout-hook: hook is not a procedure of one argument: ~S" (car hook)))           
              (error "sprout-hook: hook is not a procedure or #f: ~S" (car hook)))))
  *sprout-hook*)

(define (sprout proc . args)
  ;; (sprout {proc|list} [ahead|list] [id|list|file] ...)
  (let ((start 0)
	(id 0)
	(file #f)
        (metro 0)
        (startBeat 0)
        )
    ;; parse args and check for illegal values before sprouting
    ;; anything. first make sure all procs are really procedures...
    (or (procedure? proc)
	(and (pair? proc)
	     (do ((tail proc (cdr tail)))
		 ((null? tail) #t)
	       (if (not (procedure? (car tail)))
		   (error "~S is not a process" proc))))
	(error "~S is not a process" proc)	)
    ;; parse optkey args until file info, allow key args for start and id
    (do ((tail args (cdr tail))
         (keyw #f)  ; true if encountered keyword
         (argn 0 (+ argn 1)))
        ((or (null? tail) file)
         (set! args tail))
      (cond ((string? (car tail))
             (set! file (car tail)))
            ((keyword? (car tail))
             (set! keyw #t)
             (if (null? (cdr tail)) (error "Missing arg value for ~S" (car tail)))
             (cond ((eq? (car tail) :start)
                    (set! start (cadr tail))
                    (set! tail (cdr tail)) )
                   ((eq? (car tail) :id)
                    (set! id (cadr tail))
                    (set! tail (cdr tail)))
                   (else
                    (error "invalid keyword: ~S" (car tail))
                    )))
            (keyw 
             (error "expected key arg but got ~S instead" (car tail))
             )
            ((= argn 0)
             (set! start (car tail)))
            ((= argn 1)
             (set! id (car tail)))
            (else
             (error "illegal sprout arguments ~S" args))))

    ;; make sure all starts are valid start times...
    (if (procedure? start)
        (begin
          (set! startBeat (car (start)))
          (set! metro (cadr (start)))
          (set! start 0)))
    (or (and (number? start) (>= start 0))
	(and (pair? start)
	     (do ((tail start (cdr tail)))
		 ((null? tail) #t)
	       (if (or (not (number? (car tail)))
		       (< (car tail) 0))
		   (error "~S is not a process start time for sprout"
			  (car tail)))))
	(error "~S is not a process start time for sprout" start))
    ;; make sure all ids are integers
    (or (or (integer? id) (string? id))
	(and (pair? id)
	     (do ((tail id (cdr tail)))
		 ((null? tail) #t)
	       (if (not (or (integer? (car tail)) (string? (car tail))))
		   (error "~S is not a process id for sprout"
			  (car tail)))))
	(error "~S is not a process id" id))
    ;;make sure metro is a valid metronome
    (if (not (metro? metro))
        (error "metro ~S is not a valid metronome." metro))
    ;;make sure startBeat is a positive number
    (if (not (or (number? startBeat)
                 (>= 0 startBeat)))
        (error "~S is not a valid start beat" startBeat))
    ;; open output file or signal error
    (if file (apply open-file file args))
    (if *sprout-hook* (*sprout-hook* file))
    ;; everything ok, do process sprouting!
    (let ((nextstart
	   (lambda ()
	     (if (pair? start)
		 (let ((v (car start)))
		   (set! start (if (null? (cdr start))
				   (car start) (cdr start)))
		   v)
		 start)))
	  (nextid 
	   (lambda ()
	     (if (pair? id)
		 (let ((v (car id)))
		   (set! id (if (null? (cdr id))
				(car id) (cdr id)))
		   v)
		 id))))
      (if (pair? proc)
	  (do ((p proc (cdr p)))
	      ((null? p) proc)
	    (ffi_sched_sprout (car p) (nextstart)
			      (string-hash (nextid)) metro startBeat))
	  (ffi_sched_sprout proc (nextstart) (string-hash (nextid)) metro startBeat))
      (void))))

(define (now)
  (ffi_now))

;(define (score-time)
;  (ffi_score_time ))
;
;(define (score-mode? )
;  (ffi_score_mode_p))

(define (pause )
  (ffi_sched_pause ))

(define (paused? )
  (ffi_sched_paused_p))

(define (continue )
  (ffi_sched_continue ))

;(define (stop . procid)
;  (let ((id -1))
;  (if (null? procid)
;      (ffi_sched_stop id)
;      (do ((tail procid (cdr tail)))
;       	  ((null? tail) #f)
;        (set! id (car tail))
;       	(if (fixnum? id)
;     	    (ffi_sched_stop id)
;     	    (error "~S is not a process id" id))))
;  (void)))

;if you call stop(), it will stop everything
;if you call stop() with a user ID, it will stop that id proc
;if you call stop() with 0 as the ID, it will stop all default processes

(define* (stop . procid)
  (let ((id 0))  
    (if (null? procid)
        (ffi_sched_stop_all)
        (do ((tail procid (cdr tail)))
            ((null? tail) #f)
          (set! id (car tail))
          (if (or (fixnum? id) (string? id))
              (ffi_sched_stop (string-hash id))
              (error "~S is not a process id" id))))
    (void)))
 
;(define (hush )
;  (ffi_sched_hush))

(define (busy? )
  (ffi_sched_busy_p))

;;;
;;; metronome functions
;;;

(define *metro* 0) ;; the default metro, initially set to the system metro

(define (metro? id)
  (and (integer? id) (ffi_sched_metro_id_valid id)))

(define* (delete-metro metro)
  (if (not (metro? metro))
      (error "metro ~S is not a valid metronome." metro)
      (if (eqv? metro 0)
          (error "default metronome (0) cannot be deleted.")))
  (ffi_sched_delete_metro metro))

(define* (make-metro tempo)
  (if (or (not (number? tempo)) (<= tempo 0))
      (error "tempo ~S is not a number greater than 0." tempo))
  (ffi_sched_make_metro tempo))

(define* (metro tempo . args)
  (if (or (not (number? tempo)) (<= tempo 0))
      (error "tempo ~S is not a number greater than 0." tempo)) 
  (with-optkeys (args (secs 0) (metro *metro*) (beats #f))
    (let ((issecs #t))
;      (if (> secs 1000)
;    (print "Warning: your tempo change will occur over thousands of seconds.
;  Perhaps you forgot the metro: keyword when indicating your metro?"))
      (if beats
          (begin
            (if (or (not (number? beats)) (< beats 0))
                (error "beats ~S is not a non-negative number of seconds." beats))
            (set! secs beats)
            (set! issecs #f))
          (if (or (not (number? secs)) (< secs 0))
              (error "secs ~S is not a non-negative number of seconds." secs)))
      (if (not (metro? metro))
          (error "metro ~S is not a valid metronome." metro))   
      (ffi_sched_set_tempo tempo secs issecs metro))))


(define* (metro-dur beats (metro *metro*))
  (if (not (metro? metro))
    (error "metro ~S is not a valid metronome." metro))
  (ffi_sched_metro_dur beats metro))

(define* (metro-beat (metro *metro*))
  (if (not (metro? metro))
      (error "metro ~S is not a valid metronome." metro))
  (ffi_sched_get_metro_beat metro))

;; this function was originally called now-tempo

(define* (metro-tempo (metro *metro*))
  (if (not (metro? metro))
      (error "metro ~S is not a valid metronome." metro))
  (ffi_sched_get_metro_tempo metro))

(define* (sync (ahead 1) (metro *metro*))
  (if (<= ahead 0)
    (error "ahead ~S must be a number greater than 0." ahead))
  (if (> ahead 1000)
    (print "Warning: your process will start far into the future.
  Perhaps you forgot the metro: keyword when indicating your metro?"))
  (let* ((whole (floor ahead))
         (fract (- ahead whole)))
    (lambda()
      (let ((start-beat 0)
            (curbeat (metro-beat metro))
            (base (+ (floor (metro-beat metro)) fract)))
        (if (eqv? 0 whole) 
            (if (< curbeat base)
              (set! start-beat (floor curbeat))
              (set! start-beat (ceiling curbeat)))
          (set! start-beat (* (ceiling (/ curbeat whole)) whole)))
        (list (+ start-beat fract) metro)))))


(define* (metro-sync metro . args)
  (with-optkeys (args (beats 1) (tempo 0) (master-metro -1) (mode 0) (secs #f))
     (if (eqv? 0 (last (ffi_sched_get_metros #f)))
       (error "more than one metronome must exist in order to sync two metronomes."))
     (if (and (eqv? metro *metro*) (eqv? master-metro -1))
         (set! master-metro (first (ffi_sched_get_metros #t))))
     (if (eqv? master-metro -1)
       (set! master-metro *metro*))
     (cond ((<= beats 0)
            (error "beats ~S is not a number greater than 0." beats))
           ((and secs (<= secs 0))
            (error "secs ~S must be a positive non-zero number" beats))
           ((not (metro? metro))
            (error "metro ~S is not a valid metronome." metro))
           ((not (metro? master-metro))
            (error "metro ~S is not a valid metronome." master-metro))
           ((not (or (eqv? mode 0) (eqv? mode 1) (eqv? mode -1)))
            (error "mode ~S is not -1, 0, or 1." mode))
           ((eqv? metro master-metro)
            (error "metro and master-metro refer to the same metronome.")))
     (let ((isbeats #t))
       (when secs
          (set! beats secs)
          (set! isbeats #f))
       (ffi_sched_sync_metros metro beats master-metro tempo isbeats mode))))

(define (metro-phase fitbeats beatspace . args)
  (with-optkeys (args (metro *metro*))
    (if (not (metro? metro))
        (error "metro ~S is not a valid metronome." metro))
    (if (or (not (number? fitbeats)) (<= fitbeats 0))
        (error "fitbeats ~S is not a number greater than 0." fitbeats))
    (if (or (not (number? beatspace)) (<= beatspace 0) )
        (error "beatspace ~S is not a number greater than 0." beatspace))
    (ffi_sched_metro_phase fitbeats beatspace metro)))

(define* (metros (user #f))
  (ffi_sched_get_metros user))

