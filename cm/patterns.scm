;;; **********************************************************************
;;; Copyright (c) 2008, 2009 Rick Taube.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

;;;
;;; patterns using structs instead of classes.
;;; requires with-optkeys, arithmetic-test, list-set, tb:rani tb:ranf
;;; 

(define-constant +constant-data+    (ash 1 0)) ; avoid hair when possible
(define-constant +default-period+   (ash 1 1)) ; no period specified
(define-constant +constant-weights+ (ash 1 2)) ; avoid random index recalc
(define-constant +count-periods+    (ash 1 3)) ; period counts subperiods
(define-constant +count-values+     (ash 1 4)) ; period counts values
(define-constant +depth-first+      (ash 1 5)) ; pattern moves on eop
(define-constant +breadth-first+    (ash 1 6)) ; pattern moves each time

(define-constant +nad+ '#:nad)         ; "not a datum" marker
(define-constant +eop+ '#:eop)         ; "end of period" marker
(define-constant +eod+ '#:eod)         ; "end of data" marker

;;; the period struct holds information for period calculation.  count
;;; is number of reads remaining in current period. when count=0 the
;;; period is reinitialized. length is maximum count of the period,
;;; either a number or #t if dynamic length. if stream is not #f a new
;;; length will be read from it each time the period is initialized.
;;; omit is the number of times this stream is skipped in its parent's
;;; pattern, if dynamic. Reps keeps track of the number of
;;; periods. Max is the max number of periods allowed, after which the
;;; pattern always returns +eod+

(define-record period count length stream default omit reps )

(define (pperiod obj )
  (list 'period
	(period-count obj) (period-length obj) (period-stream obj)
	(period-default obj) (period-omit obj) (period-reps obj)
	))

(define-record pattern flags data length datum period value state 
  repeat returning counting traversing next mapr cache)

(define (ppattern obj )
  (list 'pattern
	#:flags (pattern-flags obj)
	#:data (pattern-data obj)
	#:length (pattern-length obj)
	#:datum (pattern-datum obj)
	#:period (pperiod (pattern-period obj))
	#:value (pattern-value obj)
	#:state (pattern-state obj)
	#:repeat (pattern-repeat obj)
	#:returning (pattern-returning obj)
	#:cache (pattern-cache obj)
	))

(define (%alloc-pattern)
  ;; flags data length datum period value state limit returning counting traversing next mapr cache
  (make-pattern 0 (list) #f +nad+ #f +nad+ +nad+ most-positive-fixnum #f #:periods #:depth-first
		#f #f #f))

(define (initialize-pattern obj data for rep flags len dper getr mapr)
  (pattern-data-set! obj data)
  (pattern-length-set! obj len)
  (pattern-mapr-set! obj mapr)
  (pattern-next-set! obj getr)
  ;; map data to see if it is constant data or has subpatterns
  (let ((con? #t))
    (map-pattern-data (lambda (x) (if (pattern? x) (set! con? #f))) 
		      obj)
    (if con? (set! flags (logior flags +constant-data+))))
  ;; parse counting option
  (let ((counting (pattern-counting obj)))
    (case counting
      ((#:periods )
       (set! flags (logior flags +count-periods+)))
      ((#:values )     
       (set! flags (logior flags +count-values+)))
      (else
       (error "illegal counting value ~S" counting))))
  ;; parse traversing option
  (let ((traversing (pattern-traversing obj)))
    (case traversing
      ((#:depth-first ) 
       (set! flags (logior flags +depth-first+)))
      ((#:breadth-first )
       (set! flags (logior flags +breadth-first+)))
      (else
       (error "illegal traversing value ~S" traversing))))
  ;; if constant data and counting subperiods, switch to counting
  ;; values instead since its the same thing and we can avoid
  ;; resetting subperiods if period length is nevertheless expressed
  ;; dynamically.
  (cond ((logtest flags +count-values+)
	 (set! flags (logand flags (lognot +count-periods+))))
	(else
	 (if (logtest flags +constant-data+)
	     (set! flags (logior 
			  (logand 
			   flags (lognot +count-periods+))
			  +count-values+))
	     (set! flags (logior flags +count-periods+)))))
  (pattern-repeat-set! obj (if (and (number? rep) (> rep 0))
			       rep most-positive-fixnum))
  (let ((per (or for dper)))
    ;; period not specified so mark that we are using default period
    (when (not for)
      (set! flags (logior flags +default-period+)))
    (pattern-period-set! obj
			 (if (or (number? per)
				 (eqv? per #t))
			     ;;           count len src dper omit reps
			     (make-period 0     per #f  dper 0    0)
			     ;;           count len src dper omit reps 
			     (make-period 0     #f  per dper 0    0))))
  (pattern-flags-set! obj flags)
  (values))

;;;
;;; Predicates for testing end-of-period and end-of-data.
;;;

(define (eop? x)
  (if (pattern? x)
      (eop? (pattern-state x))
      (eqv? x +eop+)))

(define (eod? x)
  (if (pattern? x)
      (eod? (pattern-value x))
      (eqv? x +eod+)))

;;;
;;; next returns the next value read from the object.  this around
;;; method implements the basic behavior of patterns.  it first checks
;;; the stream's period length and calls reset-period if at end. if
;;; the next period length is 0 it immediately returns +nad+, which
;;; causes a superior stream (if any) to skip over the current stream
;;; as it increments its pattern.  otherwise, the method then
;;; increments the streams pattern until it yields a datum that is not
;;; +nad+ and that call-next-method does not return +nad+ from. if the
;;; stream's data is known to contain only constant values, ie no
;;; substreams, the testing loop is skipped. once call-next-method
;;; returns a value (not +nad+), the period and pattern of the stream
;;; are incremented according to their mode. for period incrementing,
;;; +count-periods+ increments the period count only on +eop+, and
;;; +count-values+ increments the period count every time. for pattern
;;; incrementing, +depth-first+ increments the pattern only on +eop+,
;;; and +breadth-first+ increments the pattern every time.
;;;

(define (next obj . args)
  (let ((num (if (null? args) #f (car args))))
    (if num
      (if (number? num )
        (let ((l (list #f)))
          (do ((i 0 (+ 1 i))
               (e l (cdr e)))
              ((>= i num)
               (cdr l))
            (set-cdr! e (list (next-1 obj)))))
        (if (pattern? obj)
          (let ((l (list #f)))
            (do ((n (next-1 obj) )
                 (e l (cdr e))
                 (f #f))
                ((or (eqv? n +eod+) f)
                 (cdr l))
              (set-cdr! e (list n))
              (if (eop? obj)
                (set! f #t)
                (set! n (next-1 obj))))) 
          (list obj)))
      (next-1 obj))))

(define (next-1 obj)
  (cond ((pattern? obj)
	 (let ((period (pattern-period obj))
	       (nomore #f))
	   ;; reset period, return
	   (when (= (period-count period) 0)
		 (when (>= (period-reps period)
			   (pattern-repeat obj))
		       (pattern-value-set! obj +eod+)
		       (pattern-state-set! obj +eop+)
		       (set! nomore +eod+))
		 (when (and (not nomore)
			    (= (reset-period obj) 0))
		       (set! nomore +nad+)
		       (pattern-value-set! obj +nad+)
		       (pattern-state-set! obj +eop+)))
	   (if nomore
	       nomore
	       (let ((flags (pattern-flags obj))
		     (retfn (pattern-returning obj))
		     (value #f)
		     (state #f))
		 ;; increment datum until not +nad+
		 (if (logtest flags +constant-data+)
		     (begin
		       (pattern-datum-set! obj (next-in-pattern obj))
		       (set! value (next-1 (pattern-datum obj)))
		       (set! state +eop+)
		       ;;(print (list #:consant!))
		       )
		     (do ((dyn? (and (logtest flags +count-periods+)
				     (eqv? (period-length period) #t)))
			  (stop #f))
			 (stop #f)
		       ;; increment over 0 length substreams
		       (do ()
			   ((not (eqv? (pattern-datum obj) +nad+)) #f)
			 (pattern-datum-set! obj
					     (if dyn?
						 (skip-datum? (next-in-pattern obj))
						 (next-in-pattern obj))))
		       (set! value (next-1 (pattern-datum obj)))
		       (if (pattern? (pattern-datum obj))
			   (set! state (pattern-state (pattern-datum obj)))
			   (set! state +eop+))
		       ;; increment over +nad+ values returned by obj.
		       (if (eqv? value +nad+)
			   (pattern-datum-set! obj value)
			   (set! stop #t ))) )
		 ;; increment period and pattern as appropriate.
		 (cond ((eqv? state +eop+)
			;;(print (list #:state-eop!))
			(period-count-set! period (- (period-count period) 1))
			(pattern-datum-set! obj +nad+)
			(set! state #f))
		       (else
			(if (logtest flags +breadth-first+)
			    (pattern-datum-set! obj +nad+))
			(if (logtest flags +count-values+)
			    (period-count-set! period
					       (- (period-count period) 1)))))
		 ;;(print (list #:period-count (period-count period)))
		 (if (= (period-count period) 0)
		     (begin (set! state +eop+)
			    (period-reps-set! period
					      (+ 1 (period-reps period))))
		     (set! state state))
		 
		 (if retfn
		     (set! value ( retfn value)));; thunk
		 
		 (pattern-state-set! obj state)
		 (pattern-value-set! obj value)
		 value))))
	((procedure? obj)
	 (obj )	 )

	(else
	 obj)))

(define (next-in-pattern obj)
  ( (pattern-next obj) obj)
  )

(define (map-pattern-data fn obj)
  ( (pattern-mapr obj) fn obj)
  )

;;;
;;; skip-datum? returns +nad+ if the current stream should be skipped
;;; in the pattern. this only happens if we have dynamic periodicity
;;; and the datum had a 0 length period when it was encountered by
;;; reset-period.
;;;

(define (skip-datum? obj)
  (if (not (pattern? obj))
      obj
      (let ((period (pattern-period obj)))
	(if (> (period-omit period) 0)
	    (begin (period-omit-set! period
				     (- (period-omit period) 1))
		   +nad+)
	    obj))))

;;;
;;; reset-period sets and returns the length of the next
;;; period. period length of constant datum is always 1.
;;;

(define (reset-period obj)
  (if (not (pattern? obj)) 1
      (let ((period (pattern-period obj))
	    (dyn #f)
	    (len #f))
	
	;; if period is supplied as a stream get next length via item
	(when (period-stream period)
	  (period-length-set! period
			      (next-1 (period-stream period))))
	(set! dyn (eqv? (period-length period) #t))
	(set! len
	      (if dyn
		  (period-default period)
		  (period-length period)))
	;; if we have dynamic period length we adjust next period
	;; length for the number of 0 subperiods that this period will
	;; encounter.  in order for this to work, all substream
	;; periods must be reset now, at the same that the super
	;; stream is reset. we can only do this if we know that all
	;; subperiods are currently at end of period, ie if we are
	;; counting by subperiods. if so, then by definition all the
	;; substreams must be at end-of-period or we couldn't have
	;; gotton here in the first place. after resetting substream
	;; period lengths we decrement our current stream's period
	;; length by the number of zero periods found.
	(when (and dyn
		   (logtest (pattern-flags obj) +count-periods+))
	  (let ((zeros 0))
	    (map-pattern-data
	     (lambda (x)
	       (when (= (reset-period x) 0) 
		 (let ((p (pattern-period x)))
		   (period-omit-set! p 
				     (+ (period-omit p)
					1)))
		 (set! zeros (+ zeros 1))
		 ))
	     obj)
	    (when (> zeros 0)
	      (set! len (max (- len zeros) 0)))))
	(period-count-set! period len)

	len)))

;;;
;;; pattern implementations.
;;;
;;; cycle continously loops over its data. the data are held in a list
;;; of the form: (data . data). successive elements are popped from
;;; the cdr and when the cdr is null it's reset to the car.
;;;

(define (make-cycle data . args)
  (unless (pair? data) (set! data (list data)))
  (with-optkeys (args for limit)
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  (len (length data)))
      (initialize-pattern obj (cons data data) for limit
			  flags len len next-in-cycle
			  (lambda (fn obj) 
			    (for-each fn (car (pattern-data obj)))))
      obj)))

(define (next-in-cycle obj)
  (let ((data (pattern-data obj)))
    (if (null? (cdr data))
	(set-cdr! data (car data)))
    (let ((x (cadr data)))
      (set-cdr! data (cddr data))
      x)))

; (define aaa (make-cycle (list 1 2 3)))
; (next aaa #t)
; (define aaa (make-cycle (list 1 2 3) for: 2))
; (next aaa #t)
; (define aaa (make-cycle (list 1 2 3) for: (make-cycle (list 3 2 1))))
; (next aaa #t)
; (define aaa (make-cycle (list 1 2 3) #:limit 2))
; (next aaa #t)
; (define aaa (make-cycle (list (make-cycle (list 'a 'b) ) (make-cycle (list 1 2) ))))
; (next aaa #t)
; (define aaa (make-cycle (list 1 (make-cycle (list 'a 'b)))))
; (next aaa #t)
; (define aaa (make-cycle (list 1 (make-cycle (list 'a 'b) for: (make-cycle (list 3 2 1 0))))))
; (next aaa #t)

;;;
;;; palindrome
;;;

(define-record palin pos len inc mode elide)

(define (ppalin obj port)
  (list 'palin 
	(palin-pos obj) (palin-len obj) (palin-inc obj)
	(palin-mode obj) (palin-elide obj)))

(define (make-palindrome data . args)
  (unless (pair? data) (set! data (list data)))
  (with-optkeys (args for limit elide)
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  (len (length data)))
      (initialize-pattern obj data for limit
			  flags len (* len 2) next-in-palindrome
			  (lambda (fn obj)
			    (for-each fn (pattern-data obj))))
      ;; pattern cache holds palin structure
      (pattern-cache-set! obj (make-palin -2 (length data) #f #f
					  elide))
      obj)))

(define (next-in-palindrome obj)
  (let* ((cache (pattern-cache obj))
	 (pos (palin-pos cache)))
    (cond ((< pos 0 ) 
	   ;; starting new up-and-back cycle
	   (let ((m (next-1 (palin-elide cache)))
		 (l (palin-len cache))
		 (i (= pos -2)))
	     (palin-mode-set! cache m)
	     (palin-inc-set! cache 1)
	     ;; see if we skip repeat of first element
	     (if (or (eqv? m #t) (and (pair? m) (eqv? (car m) #t)))
		 ;; -2 marks very first call, dont skip inital element
		 (if i (set! pos 0) (set! pos 1))
		 (set! pos 0))	     
	     (if (logtest (pattern-flags obj) +default-period+)
		 (let* ((p (pattern-period obj))
			(c (* l 2)))
		   (cond ((eqv? m #f)
			  (period-count-set! p c))
			 ((eqv? m #t)
			  (period-count-set! p (if i (- c 2) (- c 3))))
			 ((equal? m '(#f #t))
			  (period-count-set! p (- c 1)))
			 ((equal? m '(#t #f))
			  (period-count-set! p (if i (- c 1) (- c 2))))
			 (else (period-count-set! p c)))
		   ))
	     ))
	  ((= pos (palin-len cache))
	   ;; reversing direction
	   (palin-inc-set! cache -1)
	   (let ((m (palin-mode cache)))
	     ;; test if we skip repeat of last element
	     (if (or (eqv? m #t) (and (pair? m) (pair? (cdr m))
				      (eqv? (cadr m) #t)))
		 (set! pos (- pos 2))
		 (set! pos (- pos 1))))
	   ))
    (palin-pos-set! cache (+ pos (palin-inc cache)))
    (list-ref (pattern-data obj) pos)))

; (define aaa (make-palindrome '(a b c d) ))
; (next aaa #t)
; (define aaa (make-palindrome '(a b c d) elide: #t))
; (next aaa #t)
; (define aaa (make-palindrome '(a b c d) elide: '(#f #t)))
; (next aaa #t)
; (define aaa (make-palindrome '(a b c d) elide: '(#t #f)))
; (next aaa #t)
; (define aaa (make-palindrome '(a b c d) for: 3))
; (next aaa #t)

;;;
;;; line sticks on the last element.
;;;

(define (make-line data . args)
  (unless (pair? data) (set! data (list data)))
  (with-optkeys (args for limit)
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  (len (length data)))
      (initialize-pattern obj data for limit flags
			  len len next-in-line
			  (lambda (fn obj)
			    (for-each fn (pattern-data obj))))
      obj)))

(define (next-in-line obj)
  (let ((line (pattern-data obj)))
    (if (null? (cdr line))
	(begin 
	  (period-count-set! (pattern-period obj) 1)
	  (car line)
	  )
	(let ((x (car line)))
	  (pattern-data-set! obj (cdr line))
	  x))))

;;; (define aaa (make-line '(a b c)))
;;; (next aaa #t)
;;; (define aaa (make-line (list 'a 'b (make-cycle '(1 2 3 4)))))
;;; (next aaa #t)
;;; (define aaa (make-line (list 'a 'b (make-cycle '(1 2 3 4) for: (lambda () (+ 1 (random 4)))))))
;;; (next aaa #t)
;;; aaa

;;;
;;; heap shuffles its elements each time through
;;;

(define (make-heap data . args)
  ;; copy data because heap destructively modifies it
  (if (pair? data)
      (set! data (append data (list)))
      (set! data (list data)))
  (with-optkeys (args for limit)
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  (len (length data)))
      (initialize-pattern obj (list data) for limit
			  flags len len next-in-heap
			  (lambda (fn obj)
			    (for-each fn (car (pattern-data obj)))))
      obj)))
  
(define (next-in-heap obj)
  (let ((data (pattern-data obj)))
    (when (null? (cdr data))
      (let ((len (pattern-length obj))
	    (lis (car data)))
	(do ((i 0 (+ i 1))
;;;	     (j (random len ) (random len))
	     (j (ffi_ranint len ) (ffi_ranint len))
	     (v #f))
	    ((= i len)
	     (set-cdr! data lis))
	  (set! v (list-ref lis i))
	  (list-set! lis i (list-ref lis j))
	  (list-set! lis j v))))
    (let ((x (cadr data)))
      (set-cdr! data (cddr data))
      x)))

;; (define xxx '(1 2 3 4))
;; (define aaa (make-heap xxx))
;; (next aaa #t)
;; xxx
;; (define aaa (make-heap (list 1 2 3 (make-cycle '(a b c)) 4 5)))
;; (next aaa #t)

;;;
;;; rotation
;;;

(define (make-rotation data . args)
  ;; copy user's data (rotation side effects data)
  (if (pair? data)
      (set! data (append data (list)))
      (set! data (list data)))
  (with-optkeys (args for limit (rotate 0))
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  (len (length data)))
      ;; cdr of data initialized now so that rotations only happen
      ;; after the first cycle.
      ;; (initialize-pattern obj data args flags len dper getr mapr allow)
      (initialize-pattern obj (cons data data) for limit
			  flags len len next-in-rotation
			  (lambda (fn obj) 
			    (for-each fn (car (pattern-data obj)))))
      ;; pattern cache holds palin structure
      (pattern-cache-set! obj rotate)
      obj)))
  
(define (next-in-rotation obj)
  (define (rotate-items items start step width end)
    (do ((i start (+ i step)))
	((not (< i end)) items)
      (let ((a (list-ref items i))
	    (b (list-ref items (+ i width))))
	(list-set! items i b)
	(list-set! items (+ i width) a))))
  (let ((data (pattern-data obj)))
    (when (null? (cdr data))
      (let ((l (car data))
	    (r (next-1 (pattern-cache obj))))
	;; start step width end
	(set-cdr! data
		  (if (pair? r)
		      (if (pair? (cdr r))
			  (if (pair? (cddr r))
			      (if (pair? (cdddr r))
				  (apply rotate-items l r)
				  (rotate-items l (car r) 
						(cadr r) (caddr r)
						;; len - width
						(- (pattern-length obj) 
						   (caddr r))))
			      (rotate-items l (car r) (cadr r) 1
					    (- (pattern-length obj) 1)))
			  (rotate-items l (car r) 1 1 
					(- (pattern-length obj) 1)))
		      (rotate-items l r 1 1 
				    (- (pattern-length obj) 1))))))
    (let ((x (car (cdr data))))
      (set-cdr! data (cddr data))
      x)))

; (define aaa (make-rotation '(a b c d)))
; (next aaa #t)
; (define aaa (make-rotation '(a b c d) rotations: '(1 2)))
; (next aaa #t)

;;;
;;; weighting chooses items using weighted selection. its data are
;;; kept in a list of the form#: ((&rest choices) . last-choice).
;;;

(define-record random-item datum index weight min max count id minmax)

(define (prandom-item obj )
  (list 'random-item
	#:datum (random-item-datum obj)
	#:index (random-item-index obj)
	#:weight (random-item-weight obj)
	#:min (random-item-min obj)
	#:max (random-item-max obj)
	#:count (random-item-count obj)
	#:id (random-item-id obj)
	#:minmax (random-item-minmax obj)))

(define (make-weighting data . args)
  (let* ((pool (canonicalize-weighting-data data))
	 (obj (%alloc-pattern))
	 (len (length pool))
	 (dper #f)
	 (const-weight #t)
	 (const-datums #t)
	 (num-patterns 0)
	 (flags 0))
    (for-each (lambda (item)
		(let ((min (random-item-min item))
		      (max (random-item-max item))
		      (wei (random-item-weight item))
		      (dat (random-item-datum item)))
		  (when (pattern? dat)
		    (set! const-datums #f)
		    (set! num-patterns (+ num-patterns 1)))
		  ;; check the stream for constant weights. if true,
		  ;; calculate the range now and set a flag so we dont
		  ;; recalulate each period.
		  (unless (number? wei)
		    (set! const-weight #f))))
	      pool)
    ;; set the default period length of an all-subpattern weighting to
    ;; 1 otherwise to the number of elements. since a weighting
    ;; pattern establishes no particular order itself, setting the
    ;; period to 1 allows the number of elements in the current period
    ;; to reflect the sub patterns.
    (set! dper (if (= num-patterns len) 1 len))
    (if const-weight (set! flags (logior flags +constant-weights+)))
    ;; pool is ((&rest choices) . last-choice) no initial last
    ;; choice. a first choice for the stream could be implemented as a
    ;; last with min=1
    (with-optkeys (args for limit)
      (initialize-pattern obj (list pool) for limit
			  flags len dper next-in-weighting
			  (lambda (fn obj)
			    (for-each (lambda (i)
					( fn (random-item-datum i)))
				      (car (pattern-data obj))))))
    ;; if we have constant weights calculate the range now as fixnums
    (if const-weight (recalc-weightings obj #t))
    obj))

;;; (define aaa (make-weighting '(a b c d e)))
;;; (next aaa #t)
;;; (define aaa (make-weighting '(a b (c weight: 10) d e)))
;;; (next aaa #t)
;;; (define aaa (make-weighting '(a b (c min: 1) d e)))
;;; (next aaa #t)

(define (canonicalize-weighting-data data)
  (define (%make-random-item w)
    (let ((item #f)
	  (args (list)))
      (cond ((pair? w)
	     (set! item (car w))
	     (set! args (cdr w)))
	    (else (set! item w)))
      (with-optkeys (args (weight 1) (min 1) max)
	(make-random-item item #f weight min max 0 #f #f))))
  (map %make-random-item data))

;;; (canonicalize-weighting-data '(a b c))
;;; (canonicalize-weighting-data '(a (b 33) c))
;;; (canonicalize-weighting-data '(a (b max: 33) c))

(define (recalc-weightings obj fix?)
  (let ((data (car (pattern-data obj)))
	(range 0.0))
    (do ((tail data (cdr tail)))
	((null? tail) #f)
      (set! range (+ range (next-1 (random-item-weight (car tail)))))
      (random-item-index-set! (car tail) range))
    (if fix?
	(do ((tail data (cdr tail))
	     (index 0)
	     (total 0))
	    ((null? tail)
	     (pattern-cache-set! obj total) )
	  (set! index (/ (random-item-index (car tail))
			 range))
	  (random-item-index-set! (car tail) index)
	  (set! total index))
	(pattern-cache-set! obj range))))

(define (next-in-weighting obj)
  ;; pool is ((&rest choices) . last-item)
  (let* ((pool (pattern-data obj))
	 (per (pattern-period obj))
	 (flags (pattern-flags obj))
         (last (cdr pool)))
    (unless (logtest flags +constant-weights+)
      ;; at beginning of new period?
      (when (= (period-count per) (period-length per))
	(recalc-weightings obj #f)))
    ;; if we have a last item with an unfulfilled :min value return it
    (if (and (not (null? last))
	     (begin
	      (random-item-count-set! last
				      (+ 1 (random-item-count last)))
	      (< (random-item-count last)
		 (random-item-min last))))
	(random-item-datum last)
	(let ((range (pattern-cache obj))
	      (choices (car pool))
	      (pick (lambda (c r)
		      (do ((tail c (cdr tail))
;;;			   (index (random r ))
			   (index (ffi_ranfloat r ))
			   )
			  ( (< index (random-item-index (car tail)))
			    (car tail)))))
	      (next #f))
	  (do ((item (pick choices range) (pick choices range)))
	      ((not (and (random-item-max item)
			 (= (random-item-count item)
			    (random-item-max item))))
	       (set! next item))
	    )
	  (unless (eqv? next last)
	    (do ((tail choices (cdr tail)))
		((null? tail) #f)
	      (random-item-count-set! (car tail) 0)))
	  (set-cdr! pool next)
	  ;; adjust the weight of the newly selected item
	  (random-item-datum next)))))

;;;
;;; markov 
;;;

(define (canonicalize-markov-data data)
  (define (parse-markov-spec spec)
    (if (not (pair? spec))
	(error "transition ~S is not a list" spec))
    (let ((rhside (or (member '-> spec)
		      (member '#:-> spec)
		      (error "no right hand side in transition ~S"
			     spec)))
	  (lhside (list))
	  (range 0) 
	  (outputs (list)))
      ;; separate lh and rh sides
      (let* ((head (list #f))
	     (tail head))
	(do ()
	    ((eqv? spec rhside)
	     (set! lhside (cdr head))
	     (set! rhside (cdr rhside)))
	  (set-cdr! tail (list (car spec)))
	  (set! tail (cdr tail))
	  (set! spec (cdr spec))))
      (for-each (lambda (s)
		  (let ((val #f)
			(pat #f)
			(wei #f))
		    (if (pair? s)
			(begin (set! val (car s))
			       (set! wei (if (null? (cdr s)) 1 (cadr s)))
			       ;; weight may be number or pattern
			       (set! pat wei)
			       (unless (number? wei)
				 (set! wei #f)))
			(begin (set! val s) (set! wei 1) (set! pat 1)))
		    ;; set range to #f if any weight is pattern
		    ;; else precalc range for the constant weights
		    (if (and wei range)
			(set! range (+ range wei))
			(set! range #f))
		    ;;(push (list val range pat) outputs)
		    (set! outputs (cons (list val range pat) outputs))
		    ))
		rhside)
      (cons lhside (cons range (reverse outputs)))))
    (let ((transitions (list #f)))
      (do ((tail data (cdr tail))
	   (order #f)
	   (lis transitions)
	   (p #f))
	  ((null? tail)
	   (cdr transitions) )
	(set! p (parse-markov-spec (car tail)))
	(if (not order)
	    (set! order (length (car p)))
	    ;;(set! order (max order (length (first p))))
	    (if (not (= order (length (car p))))
		(error "found left hand sides with different number of items in ~S" 
		       data))
	    )
	(set-cdr! lis (list p))
	(set! lis (cdr lis)))))

;;; (parse-markov-spec '(a a -> b  c ))
;;; (canonicalize-markov-data '((a a -> b  c ) ( a b -> a) (c a -> c a)))

(define (make-markov data . args)
  (if (not (pair? data))
      (error "~S is not list of markov transitions" data)
      (set! data (canonicalize-markov-data data)))
  (with-optkeys (args for limit past)
    (let* ((obj (%alloc-pattern))
	   (len (length data))
	   (flags 0))
      (initialize-pattern obj data for limit
			  flags len len next-in-markov
			  (lambda (fn obj)
			    (for-each fn (pattern-data obj))))
      (unless (pair? past)
	(set! past (make-list (length (car (car data))) '*)))
      (pattern-cache-set! obj past)
      obj)))
  
(define (next-in-markov obj)
  ;; markov data kept as a list of lists. each list is in the form#:
  ;; ((<inputs>) range . <output>)
  (letrec ((select-output
            (lambda (range outputs)
              ;; if range is #f then one or more weights in the
              ;; outputs are patterns. in this case we map all the
              ;; outputs to update weights of every outcome and then
              ;; select.  otherwise (range is number) we simply select
              ;; an outcome from the precalculated distribution.
              (if (not range)
		  (do ((tail outputs (cdr tail))
		       (out #f)
		       (sum 0))
		      ((null? tail)
		       (select-output sum outputs))
		    ;; out is outcome#: (val rng <pat/wei>)
		    (set! out (car tail))
		    ;; if third element is number use it else read it
		    (set! sum (+ sum (if (number? (caddr out))
					 (caddr out)
					 (next-1 (caddr out)))))
		    ;; always update second element to new value
		    (set-car! (cdr out) sum))
                (let (
;;;		      (n (random range))
		      (n (ffi_ranfloat range))
		      )
		  (do ((tail outputs (cdr tail)))
		      ((< n (cadr (car tail)))
		       (car (car tail)))))
		)))
	   (match-past
	    (lambda (inputs past)
	      (do ((i inputs (cdr i))
		   (j past (cdr j))
		   (f #t))
		  ((or (null? i) (null? j) (not f))
		   f)
		(set! f (or (eqv? (car i) '*)
			    (equal? (car i) (car j))
			    (eqv? (car j) '*))))
	      )))
    (do ((tail (pattern-data obj) (cdr tail))
	 (past (pattern-cache obj))
	 (item #f)
	 )
	((or (null? tail) (null? past) 
	     (match-past (car (car tail)) past))
	 (when (null? tail)
	   (error "no transition matches past ~S"  past))
	 (set! item (select-output (cadr (car tail))
				   (cddr (car tail))))
	 (unless (null? past)
	   (if (null? (cdr past))
	       (set-car! past item)
	       (do ((last past (cdr last)))
		   ((null? (cdr last))
		    ;; rotate past choices leftward
		    (set-car! past item)
		    (set-cdr! last past)
		    (pattern-cache-set! obj (cdr past))
		    (set-cdr! (cdr last) (list))))))
	 item))
    ))
   
;;; (define aaa (make-markov '((a -> b c d) (b -> a) (c -> d) (d -> (a 3) b c))))
;;; (next aaa 30)


(define (markov-analyze seq . args) 
  (let* ((morder #f) ; markov order
	 (result #f) ; what to return
	 (len (length seq)) 
	 (labels '())			; the set of all outcomes 
	 (table '())
	 (row-label-width 8) 
	 (print-decimals 3)
	 (field (+ print-decimals 2)))	; n.nnn 
    (with-optkeys (args (order 1) (mode 1))
      (set! morder order)
      (set! result mode))
    (unless (member result '(1 2 3))
      (error "~S is not a valid mode value" result))
    (letrec ((add-outcome
	      (lambda (prev next) 
		(let ((entry (list-find (lambda (x)
					  (equal? prev (car x)))
				   table)))
		  (if (not entry) 
		      (set! table (cons (list prev
					      (format #f "~s" prev) 
					      (list next 1))
					table)) 
		      (let ((e (assoc next (cddr entry)))) 
			(if e 
			    (set-car! (cdr e) (+ 1 (cadr e)))
			    (set-cdr! (tail (cdr entry))
				      (list (list next 1)))))))))
	     (before?
	      (lambda (x y l) 
		(if (null? x) #t 
                    (let ((p1 (list-index (lambda (z) (equal? (car x) z))
					  l)) 
                          (p2 (list-index (lambda (z) (equal? (car y) z))
					  l)))
                      (cond ((< p1 p2) #t) 
                            ; bug!
                            ;((= p1 p2) (before? (cdr x) (cdr y) l)) 
                            (else #f))))))
	     (liststring 
	      (lambda (l)
		(if (null? l) ""
		    (let ((a (format #f "~a" (car l))))
		      (do ((x (cdr l) (cdr x)))
			  ((null? x) a)
			(set! a
			      (string-append 
			       a (format #f " ~a" (car x))))))))))
      (do ((i 0 (+ i 1)))
	  ((= i len) #f)
	(do ((prev (list))
	     (j 0 (+ j 1))  ; j to morder
	     (x #f))
	    ((> j morder)
	     (add-outcome (reverse prev) x ) 
	     (if (not (member x labels))
		 (set! labels (cons x labels))))
	  (set! x (list-ref seq (modulo (+ i j) len)))
	  ;; gather history in reverse order 
	  (when (< j morder) (set! prev (cons x prev)))))
      ;; sort the outcomes according to data
      (cond ((number? (car labels))
	     (set! labels (sort labels <)))
	    ((and (car labels) (symbol? (car labels)))
	     (set! labels (sort labels
				(lambda (x y) 
				  (string-ci<? (format #f "~a" x)
					       (format #f "~a" y))))))
	    (else 
	     (set! labels (reverse labels))))
      ;; map over data, normalize weights 
      (do ((tail table (cdr tail))
	   (len 0))
	  ((null? tail)
	   (set! row-label-width (max len row-label-width)) )
	(let* ((row (car tail))
	       (lab (cadr row))	; label
	       (val (cddr row)))
	  (set! len (max len (string-length lab)))
	  (let ((total (do ((e val (cdr e)) ; sum all e
			    (s 0))
			   ((null? e) s)
			 (set! s (+ s (cadr (car e))))))) 
	    (set! total (* total 1.0)) 
	    (do ((e val (cdr e)))
		((null? e) #f)
	      (set-car! (cdr (car e))
			(decimals (/ (cadr (car e)) total) 
				  print-decimals))))))
      ;; sort table by labels
      (set! table 
	    (sort table (lambda (x y) (before? (car x) (car y) labels)))) 
      ;; print table
      (when (eqv? result 1)
	(let* ((port (open-output-string))
               (sp " ")
	       (ln (make-string field #\-))) 
	  ;; print column header row
	  (newline port)
	  (do ((i 0 (+ i 1)))
	      ((= i row-label-width) #f)
	    (write-char #\* port))
	  (do ((l labels (cdr l)))
	      ((null? l) #f)
	    (display sp port) ;; column separator
	    (let* ((s (format #f "~a" (car l)))
		   (n (string-length s)))
	      ;; write column pad
	      (do ((i 0 (+ i 1))
		   (m (max (- field n) 0)))
		  ((= i m) #f)
		(write-char #\space port))
	      (display s port)))
	  ;; print each row
	  (do ((tail table (cdr tail)))
	      ((null? tail) #f)
	    (let ((row (car tail)))
	      (newline port)
	      (let* ((s (liststring (car row)))
		     (n (string-length s)))
		;; print left pad for row label
		(do ((i 0 (+ i 1))
		     (m (max (- row-label-width n) 0)))
		    ((= i m) #f)
		  (write-char #\space port))
		;; print row label min row-label-width.
		(do ((i 0 (+ i 1))
		     (m (min row-label-width n)))
		    ((= i m) #f)
		  (write-char (string-ref s i) port)))
	      (do ((l labels (cdr l)))
		  ((null? l) #f)
		(let ((v (assoc (car l) (cddr row))))
		  (if (not v)
		      (begin (display sp port) (display ln port))
		      (let* ((s (number->string (cadr v)))
			     (n (string-length s)))
			(display sp port)
			;; s7: trim number to fit field
			(if (>= n field)
			    (let ((d (position #\. s)))
			      (set! s (substring s 0 (min (+ d 4) n)))
			      (set! n (string-length s))))
			;; pad number
			(do ((i 0 (+ i 1))
			     (m (max (- field n) 0)))
			    ((= i m) #f)
			  (write-char #\space port))
			(display s port)
			))))))
	  (newline port)
          (print-output (get-output-string port))
          (close-output-port port)
          )))

    (if (= result 1)
	(void)
	;; if returning pattern or data convert table to markov lists
	(let ((pat (map (lambda (row)
			  (append (car row) '(->) (cddr row)))
			table)))
	  (if (= result 2)
	      (make-markov pat)
	      pat)))))

; (define aaa '(c4 c4 d4 c4 f4 e4 c4 c4 d4 c4 g4 f4 c4 c4 c5 a4 f4 e4 d4 bf4 bf4 a4 f4 g4 f4))
; (markov-analyze aaa 1)

;;;
;;; Graph
;;;

(define-record graph-node datum to id)

(define (pgraph-node obj port)
  (list 'graph-node
	(graph-node-datum obj) (graph-node-to obj)
	(graph-node-id obj)))

(define (make-graph data . args)
  (if (not (pair? data))
      (error "~S is not a list of graph data" data)
      (set! data (canonicalize-graph-data data)))
  (with-optkeys (args for limit)
    (let* ((obj (%alloc-pattern))
	   (len (length data))
	   (flags 0))
      (initialize-pattern obj (cons #f data ) for limit
			  flags len len next-in-graph
			  (lambda (fn obj)
			    (for-each (lambda (n) ( fn (graph-node-datum n)))
				      (cdr (pattern-data obj)))))
      obj)))

(define (canonicalize-graph-data data)
  (let ((pos 1))
    (define (parse-graph-item extern)
      (unless (pair? extern) 
	(error "~S is not a graph node list" extern))
      (apply (lambda (item . args)
	       (with-optkeys (args to id)
		 (unless id (set! id pos))
		 (set! pos (+ pos 1))
		 (make-graph-node item to id)))
	     extern))
    (map parse-graph-item data)))

;; (canonicalize-graph-data '((a to: 2) (b id: 2 to: a)))

(define (next-in-graph obj)
  (let* ((graph (pattern-data obj))
         (nodes (cdr graph))
         (this (car graph)))
    (if (not this)
	(begin
	  (set-car! graph (car nodes))
	  (graph-node-datum (car nodes)))
	;; read the to: link and search for next node
	(let ((link (next-1 (graph-node-to this)))
	      (next #f))
	  (do ((tail nodes (cdr tail)))
	      ((or next (null? tail))
	       (if (not next)
		   (error "no graph node for id ~S" link)
		   (set-car! graph next))
	       (graph-node-datum next))
	    (if (eqv? link (graph-node-id (car tail)))
		(set! next (car tail))))))))

;;; (define aaa (make-graph '((a to: b) (b to: a))))
;;; (next aaa)
;;; (define aaa (make-graph `((a to: b) (b to: c) (c ,(make-weightings '(a b c))))))
;;; (next aaa)


;;;
;;; Repeater
;;;

(define (make-repeater pat . args)
  (with-optkeys (args for repeat limit)
    (let ((obj (%alloc-pattern))
	  (flags 0)
	  )
      (initialize-pattern obj (list) for stop
			  flags
			  0
			  1
			  next-in-repeater
			  (lambda (fn obj)
			    (for-each fn (pattern-data obj))))
      ;; pattern cache holds palin structure
      (pattern-cache-set! obj (list pat repeat))
      obj)))

(define (next-in-repeater obj)
  (let ((data (pattern-data obj)))
    (if (null? data)
	(let* ((per (pattern-period obj))
	       (res (next (car (pattern-cache obj)) #t))
	       (len (length res))
	       (for (period-length per))
	       (rep (cadr (pattern-cache obj))))
	  (if rep
	      (begin
		(set! for (next rep))
		(period-length-set! per len)
		(period-count-set! per len))
	      (period-count-set! per (* len for)))
	  (let ((sav res)
		(don (- for 1)))
	    (do ((i 0 (+ i 1)))
		((not (< i don)) #f)
	      (set! res (append res sav))))
	  (pattern-data-set! obj (cdr res))
	  (car res))
	(begin
	  (pattern-data-set! obj (cdr data))
	  (car data)))))

