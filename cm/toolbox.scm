;;; **********************************************************************
;;; Copyright 1999-2013 Rick Taube.  All rights reserved.
;;; Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
;;; Public License, which says that non-commercial users may share and
;;; modify this code but must give credit and share improvements. For
;;; complete terms please read the text of the full license available at
;;; this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html
;;; **********************************************************************

(define (rescale x x1 x2 y1 y2 . b)
  (if (list? x)
      (map (lambda (z) (apply rescale z x1 x2 y1 y2 b)) x)
      (if (null? b)
	  (ffi_rescale x x1 x2 y1 y2 1)
	  (ffi_rescale x x1 x2 y1 y2 (car b)))))

(define (discrete x x1 x2 i1 . args)
  ;; formals1: x x1 x2 i1   . i2  exp)
  ;; formals2: x x1 x2 list . len exp)
  (let ((i2 #f)
	(seq #f)
	(exp 1)) 
    (cond ((pair? i1)
	   (set! seq i1)
	   (if (null? args)
	       (set! i2 (length seq))
	       (begin (set! i2 (car args))
		      (if (not? (null? (cdr args)))
			  (set! exp (cadr args))))))
	  (else
	   (set! i2 (car args))
	   (if (not (null? (cdr args)))
	       (set! exp (cadr args)))))
    (if seq
	(if (list? x)
	    (map (lambda (z)
		   (list-ref seq (ffi_discrete z x1 x2 0 i2 exp))) x)
	    (list-ref seq (ffi_discrete x x1 x2 0 i2 exp)))
	(if (list? x) 
	    (map (lambda (z) (ffi_discrete z x1 x2 i1 i2 exp)) x)
	    (ffi_discrete x x1 x2 i1 i2 exp)))))

(define (int f)
  (if (list? f)
      (map ffi_float_to_fixnum f)
      (ffi_float_to_fixnum f)))

(define (float n)
  (if (list? n)
      (map (lambda (x) (* x 1.0)) n)
      (* n 1.0)))

(define (log10 n)
  (ffi_log_ten n))

(define (log2 n)
  (ffi_log_two n))

;; arith functions that map lists.

(define (plus . args)
  ;; (plus ...) (plus '(...)) (plus '(...) n) (plus n '(...))
  (if (null? args) 
      0
      (if (null? (cdr args))
	  (if (list? (car args))
	      (apply + (car args))
	      (apply + args))
	  (if (pair? (car args))
	      (map (lambda (x) (+ x (cadr args))) (car args))
              (if (and (number? (car args)) (pair? (cdr args)) (pair? (cadr args)) (null? (cddr args)))
                  (map (lambda (x) (+ x (car args))) (cadr args))
                  (apply + args))))))

(define (times . args)
  ;; (times ...) (times '(...)) (times '(...) n) (times n '(...))
  (if (null? args) 
      1
      (if (null? (cdr args))
	  (if (list? (car args))
	      (apply * (car args))
	      (apply * args))
	  (if (pair? (car args))
	      (map (lambda (x) (* x (cadr args))) (car args))
              (if (and (number? (car args)) (pair? (cdr args)) (pair? (cadr args)) (null? (cddr args)))
                  (map (lambda (x) (* x (car args))) (cadr args))
                  (apply * args))))))

(define (minus arg . args)
  (if (null? args)
      (if (list? arg)
	  (apply - arg)
	  (- arg))
      (if (list? arg)
	  (map (lambda (x) (- x (car args))) arg)
          (if (and (pair? args) (pair? (car args)))
              (map (lambda (n) (- arg n)) (car args))
              (apply - arg args)))))

(define (divide arg . args)
  (if (null? args)
      (if (list? arg)
	  (apply / arg)
	  (/ arg))
      (if (and (list? arg) (number? (car args)))
	  (map (lambda (x) (/ x (car args))) arg)
          (if (and (number? arg) (list? (car args)))
              (map (lambda (x) (/ arg x)) (car args))
              (apply / arg args)))))

(define (less? a . args)
  ;; (less? ...) (less? '(...))
  (if (pair? a)
    (if (null? args)
        (apply < a)
        (error "less?: too many arguments: ~S" 
               (cons a args)))
    (apply < a args)))

(define (greater? a . args)
  (if (pair? a)
    (if (null? args) 
        (apply > a)
        (error "greater?: too many arguments: ~S" 
               (cons a args)))
    (apply > a args)))

(define (less=? a . args)
  (if (pair? a)
    (if (null? args) 
        (apply <= a)
        (error "less=?: too many arguments: ~S" 
               (cons a args)))
    (apply <= a args)))

(define (greater=? a . args)
  (if (pair? a)
    (if (null? args)
        (apply >= a)
        (error "greater=?: too many arguments: ~S"
               (cons a args)))
    (apply >= a args)))

(define (mod num . modu)
  (if (pair? num)
      (begin
        (set! modu (car modu))
        (map (lambda (n) (modulo n modu)) num))
      (modulo num (car modu))))

(define (minimum num . nums)
  (if (null? nums)
      (if (pair? num) (apply min num)
          (if (number? num) num
              (error "not a list or number: ~S" num)))
      (apply min num nums)))

(define (maximum num . nums)
  (if (null? nums)
      (if (pair? num) (apply max num)
          (if (number? num) num
              (error "not a list or number: ~S" num)))
      (apply max num nums)))

(define (deltas . args)
  (if (null? args)
      (list)
      (if (list? (car args))
          (apply deltas (car args))
          (if (null? (cdr args))
              (list)
              (let* ((head (list #f))
                     (tail head))
                (do ((temp args (cdr temp)))
                    ((null? (cdr temp))
                     (cdr head))
                  (set-cdr! tail (list (- (cadr temp) (car temp))))
                  (set! tail (cdr tail))))))))

(define (quantize num steps)
  (if (list? num)
      (map (lambda (n) (ffi_quantize n steps)) num)
      (ffi_quantize num steps)))

;; rounding in C++ version does not work.

(define (decimals value places)
  (let ((n (expt 10.0 places)))
    (if (list? value)
	(map (lambda (v) (/ (round (* v n)) n)) value)
	(/ (round (* value n)) n))))

(define (rhythm->seconds beats . args)
  (with-optkeys (args (tempo 60.0) (beat .25))
    (if (list? beats)
	(map (lambda (x) (ffi_rhythm_to_seconds x tempo beat))
	     beats)
	(ffi_rhythm_to_seconds beats tempo beat))))

(define (ratio->cents num)
  (if (list? num)
      (map ffi_scaler_to_cents num)
      (ffi_scaler_to_cents num)))

(define (ratio->steps num)
  (if (list? num)
      (map ffi_scaler_to_steps num)
      (ffi_scaler_to_steps num)))

(define (cents->ratio cents)
  (if (list? cents)
      (map ffi_cents_to_scaler cents)
      (ffi_cents_to_scaler cents)))

(define (interp1 x coords base)
  (let* ((x1 (if (null? coords)
		 (error "~S is an empty x y coordinate list" coords)
		 (car coords)))
	 (y1 (if (null? (cdr coords))
		 (error "malformed x y coordinate list ~S" coords)
		 (cadr coords)))
	 (x2 x1)
	 (y2 y1)
	 (b base))
    (do ((tail (cddr coords) (cddr tail)))
	((or (null? tail) (> x2 x))
	 (ffi_rescale x x1 x2 y1 y2 b))
      (set! x1 x2)
      (set! y1 y2 )
      (set! x2 (car tail)) 
      (if (null? (cdr tail))
	  (error "malformed an x y coordinate list ~S" coords)
	  (set! y2 (cadr tail))))))

(define (interp x . args)
  (if (null? args) (error "empty x y coordinate list ~S" args))
  (if (pair? (car args))
      (interp1 x (car args) 
	       (if (null? (cdr args)) 1 (cadr args)))
      (interp1 x args 1)))

(define (tendency x low high . args)
  (let ((rgen ran))
    (if (not (null? args))
	(begin (set! rgen (car args)) (set! args (cdr args))))
    (if (pair? low) (set! low (interp1 x low 1)))
    (if (pair? high) (set! high (interp1 x high 1)))
    (if (= low high)
	low
	(+ low (apply rgen
		      (if (> low high) (- low high) (- high low))
		      args)))))

;; lists

(define (list-intersection list1 list2 . args)
  (with-optkeys (args (test equal?) (getter (lambda (x) x)))
    (let* ((results (list #f))
           (backend results))
      (do ((tail1 list1 (cdr tail1)))
          ((null? tail1) (cdr results))
        (do ((item1 (getter (car tail1)))
             (item2 #f)
             (tail2 list2 (cdr tail2)))
            ((null? tail2)
             )
          (set! item2 (getter (car tail2)))
          (if (test item1 item2)
              (begin (set-cdr! backend (list (car tail2)))
                     (set! backend (cdr backend)))))))))

(define (list-difference list1 list2 . args)
 (with-optkeys (args (test equal?) (getter (lambda (x) x)))
   (let* ((results (list #f))
          (backend results)
          (diffadd (lambda (lista listb)
                     ;; add items in lista if they are not in listb
                     (do ((tail1 lista (cdr tail1))
                          (dupl? #f #f))
                         ((null? tail1) 
                          )
                       (do ((item1 (getter (car tail1)))
                            (item2 #f)
                            (tail2 listb (cdr tail2)))
                           ((or (null? tail2) dupl?)
                            )
                         (set! item2 (getter (car tail2)))
                         (set! dupl? (test item1 item2)) )
                       (if (not dupl?)
                           (begin (set-cdr! backend (list (car tail1)))
                                  (set! backend (cdr backend))))))))
     (diffadd list1 list2)
     (diffadd list2 list1)
     (cdr results))))
          
; (list-difference '() '())
; (list-difference '(1) '(1))
; (list-difference '(1) '())
; (list-difference '() '(1))
; (list-difference '(1 2) '(3 4))
; (list-difference '(0 1 2 3 4) '(2 3 4 5 6))

; (list-union '(a a v d) '(v a 4))
(define (list-union list1 list2 . args)
  (with-optkeys (args (test equal?) (getter (lambda (x) x)))
    (let* ((results (list #f))
           (backend results)
           (unionadd (lambda (list1)
                       ;; add items in lista if they are not in listb
                       (do ((tail1 list1 (cdr tail1))
                            (dupl? #f #f))
                           ((null? tail1) 
                            )
                         (do ((item1 (getter (car tail1)))
                              (item2 #f)
                              (tail2 (cdr results) (cdr tail2)))
                             ((or (null? tail2) dupl?)
                              )
                           (set! item2 (getter (car tail2)))
                           (set! dupl? (test item1 item2)) )
                         (if (not dupl?)
                             (begin (set-cdr! backend (list (car tail1)))
                                    (set! backend (cdr backend))))))))
      (unionadd list1 )
      (unionadd list2 )
      (cdr results))))
         
(define (remove-duplicates listtocheck . args)
  (with-optkeys (args (test equal?) (getter (lambda (x) x)))
    (let* ((results (list #f))
           (backend results))
      (do ((tail1 listtocheck (cdr tail1))
           (dupl? #f #f))
          ((null? tail1) (cdr results))
        (do ((item1 (getter (car tail1)))
             (item2 #f)
             (tail2 (cdr results) (cdr tail2)))
            ((or (null? tail2) dupl?)
             )
          (set! item2 (getter (car tail2)))
          (set! dupl? (test item1 item2)))
        (if (not dupl?)
            (begin (set-cdr! backend (list (car tail1)))
                   (set! backend (cdr backend)))))
      )))

;; transformations

(define (golden num1 num2 . inv?)
  (let ((gold (if (and (pair? inv?) (car inv?)) 0.38196601125001 0.61803398874999)))
    (+ (* (- num2 num1) gold) num1)))

;(define (fibonacci n)
;  (if (< n 2)
;      n
;      (+ (fibonacci (- n 1)) (fibonacci(- n 2)))))

(define (fibonacci n)
  (do ((i 0 (+ i 1))
       (p 0)
       (q 1)
       (x 0)
       (y 0))
      ((not (< i n)) p)
    (set! x p)
    (set! y q)
    (set! p y)
    (set! q (+ x y))))

;(loop for i below 10 collect (fibonacci i))

(define (fit num lb ub . mode)
  (if (null? mode) (set! mode 1)
      (set! mode (car mode)))
  (if (> lb ub) ;; rotate args
      (let ((x lb))
	(set! lb ub)
	(set! ub x)))
  (define (fit1 num lb ub mode)
    (if (<= lb num ub) num
	(let ((b (if (> num ub) ub lb))
	      (r (- ub lb)))
	  (cond ((eqv? mode 1 ) ;; WRAP
		 (+ (if (= b ub) lb ub)
		    (remainder (- num b) r)))
		((eqv? mode 2) ;; REFLECT
		 (let* ((2r (* 2 r))
			(v (remainder (- num b) 2r)))
		   (+ (if (> (abs v) r)
			  ( (if (>= v 0) - +)
			    v 2r)
			  (- v))
		      b)))
		((eqv? mode 3) ;; LIMIT
		 b)
		(else
		 (error "mode value ~S is not 1 2 or 3" mode))))))
  (if (pair? num)
      (map (lambda (n) (fit1 n lb ub mode)) num)
      (fit1 num lb ub mode)))

(define (scale len keyn . args)
  ;; args is (steps...) or ((steps...) [limit] )
  (let ((head (list #t))
	(limit #f)
	(mode 1))
    (if (pair? (car args))
	(if (null? (cdr args))
	    (set! args (car args))
	    (begin (set! limit (cadr args))
		   (if (= limit keyn)
		       (error "limit ~S same as start" limit))
		   (set! args (car args)))))
    (do ((i 0 (+ i 1))
         (k keyn)
         (l (length args))
         (t head))
        ((not (< i len)) (cdr head))
      (if limit (set! k (fit k keyn limit mode)))
      (set-cdr! t (list k))
      (set! t (cdr t))
      (set! k (+ k (list-ref args (modulo i l))))
      )))

(define (segs num sum . args)
  (if (< num  1)
      (list)
      (let ((mode 1))
	(if (pair? args)
	    (begin (set! mode (car args))
		   (set! args (cdr args))))
	(cond ((or (eqv? mode 1) (eqv? mode 2)) ; expl or geo
	       (let* ((func (if (eqv? mode 1) ffi_explseg ffi_geoseg))
		      (base (if (null? args) 2 (car args)))
		      (head (list #f))
		      (tail head))
		 (if (< base 0) (error "~S is not a valid base" base))
		 (do ((i 0 (+ i 1)))
		     ((= i num) (cdr head))
		   (set-cdr! tail
			     (list ( func i num sum base)))
		   (set! tail (cdr tail)))))
	      ((eqv? mode 3)
	       (let ((rgen ran))
		 (if (not (null? args))
		     (begin (set! rgen (car args))
			    (set! args (cdr args))))
		 (let* ((rsum (apply rgen args))
			(head (list rsum))
			(tail head))
		   (do ((i 1 (+ i 1))
			(n #f))
		       ((not (< i num))
			(do ((tail head (cdr tail)))
			    ((null? tail) head)
			  (set-car! tail
				    (ffi_rescale (car tail)
						0 rsum 0 sum 1))))
		     (set! n (apply rgen args))
		     (set-cdr! tail (list n))
		     (set! tail (cdr tail))
		     (set! rsum (+ rsum n))))))
	      (else
	       (error "~S is not a valid mode" mode))))))

;;; randomnesss

;(define ran-set! ffi_ranseed)

(define (random-seed . seed)
  (if (not (null? seed))
      (ffi_set_random_seed (car seed)))
  (ffi_get_random_seed))

(define (random-seed-set! seed)
  (ffi_set_random_seed seed)
  )

(define (ran . num)
  (if (null? num) 
      (set! num 1)
      (set! num (car num)))
  (if (> num 1)
      (if (fixnum? num) (ffi_ranint num)
	  (ffi_ranfloat num))
      (ffi_ranfloat num)))

(define (ran64 )
  (ffi_ran64))

(define (between a b)
  (if (and (fixnum? a) (fixnum? b))
      (ffi_ranint2 a b)
      (ffi_ranfloat2 a b)))

(define (pick . args)
  (if (null? (cdr args))
      (list-ref (car args) (ffi_ranint (length (car args))))
      (list-ref args (ffi_ranint (length args)))))

(define (odds n . args)
  (with-optkeys (args (true #t) (false #f))
    (if (< (ffi_ranfloat 1.0) n) true false)))

(define (vary val vari . mode)
  (if (null? mode) (set! mode 0)
      (set! mode (car mode)))
  (if (not (member mode '(0 -1 1)))
      (error "~S is not a valid mode" mode))
  (define (vary1 val vari mode)
    (if (or (<= vari 0) (= val 0))
	val
	(let* ((r (abs (* val vari)))
	       (v (ffi_ranfloat r )))
	  (if (eqv? mode 0)
	      (+ (- val (* r .5)) v)
	      (if (eqv? mode 1) (+ val v) (- val v)))
	  )))
  (if (list? val)
      (map (lambda (v) (vary1 v vari mode)) val)
      (vary1 val vari mode)))

;(define (shuffle! l)
;  (let ((s (length l)))
;    (do ((i 0 (+ i 1))
;	 (j (ffi_ranint s) (ffi_ranint s))
;	 (v #f))
;	((= i s) l)
;      (set! v (list-ref l i))
;      (set-car! (list-tail l i)
;		(list-ref l j))
;      (set-car! (list-tail l j) v))))
; michael: I just happened to do some reading on random shuffling and noted that I think the CM shuffling algorithm is slightly biased. Take a look at this page:
;
;http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#Implementation_errors
;
;The second paragraph explains the problem. I tested the statistics with the old version and for permutations of the list '(1 2 3) there is indeed a bias.

(define (shuffle! l)
  (let ((s (length l)))
    (do ((i s)
      	 (j #f)
      	 (v #f))
    	((= i 1) l)
      (set! j (ffi_ranint i))
      (set! i (- i 1))
      (set! v (list-ref l i))
      (set-car! (list-tail l i)
                (list-ref l j))
      (set-car! (list-tail l j) v))))

(define (shuffle . args)
  (if (null? (cdr args))
      (shuffle! (append (car args) (list)))
      (shuffle! (append args (list)))))

(define (drunk n width . args)
  (with-optkeys (args (low most-negative-fixnum) 
                      (high most-positive-fixnum) (mode :reflect) avoid)
    (let* ((mini (- width))
           (maxi (+ width 1))
           (incr (let loopy ((x (between mini maxi)))
                   (if (equal? x avoid) (loopy (between mini maxi)) x))))
      (set! n (+ n incr))
      (if (not (<= low n high))
          (cond ((or (eq? mode :reflect) (equal? mode -1))
                 (set! n (fit n low high)))
                ((or (eq? mode :stop) (equal? mode 0))
                 (set! n #f))
                ((or (eq? mode :limit) (equal? mode 1))
                 (set! n (max low (min n high))))
                ((or (eq? mode :reset) (equal? mode 2))
                 (set! n (+ low (/ (- high low) 2))))
                ((or (eq? mode :jump) (equal? mode 3))
                 (set! n (between low high)))
                (else 
                 (error "~S is not a valid mode" mode))))
      n)))

;; randomness
;; non-uniform distibutions

(define ranlow ffi_ranlow)

(define ranhigh ffi_ranhigh)

(define ranmiddle ffi_ranmiddle)

(define (ranbeta . args)
  ;; args: a b
  (with-optkeys (args (a .5) b)
    (if (not b) (set! b a))
    (ffi_ranbeta a b)))

(define (ranexp . args)
  ;; args: lambda
  (if (null? args)
      (ffi_ranexp 1)
      (ffi_ranexp (car args))))

(define (rangauss . args)
  ;; args: sigma mu
  (if (null? args)
      (ffi_rangauss 1 0)
      (if (null? (cdr args))
	  (ffi_rangauss (car args) 0)
	  (ffi_rangauss (car args) (cadr args)))))

(define rancauchy ffi_rancauchy)

(define (ranpoisson . args)
  ;; args: lambda
  (if (null? args)
      (ffi_ranpoisson 1)
      (ffi_ranpoisson (car args))))

(define (rangamma . args)
  ;; args: k
  (if (null? args)
      (ffi_rangamma 1)
      (ffi_rangamma (car args))))

(define ranbrown ffi_ranbrown)

(define ranpink ffi_ranpink)

(define (random-series num low high . args )
  (with-optkeys (args (series (list)) (reject #f) (sorter #f) (chooser random))
    (cond ((and (list? series) (every? number? series))
           )
          ((number? series)
           (set! series (list series)))
          (else
           (error "random-series: series is not a list of numbers: ~S" series)))
    (if (not reject) (set! reject (lambda (x l) #f)))
    (do ((count (length series))
         (limit 0)
         (rnum (+ low (chooser (- high low ))) 
               (+ low (chooser (- high low )))))
        ((or (not (< count num)) (not (< limit 100)))
         (if (not sorter) series  (sort series sorter)))
      (if (not (reject rnum series))
          (begin (set! series (cons rnum series))
                 (set! count (+ count 1))
                 (set! limit 0))
          (begin (set! limit (+ limit 1)))))))


;*************************************************************************

(define *notes* (make-equal-hash-table))


;; S7 BUG
;; begin ;define (create-note-entries )
(begin
  (let ((degrees '(("c" "bs" ) ("df" "cs") ("d"  ) ("ef" "ds" )
		   ("e"  "ff") ("f" "es" ) ("fs" "gf" ) ("g"  )
		   ("af" "gs") ("a" ) ("bf" "as" ) ("b" "cf" )))
	(octaves '("00" "0" "1" "2" "3" "4" "5" "6"
		   "7" "8" "9"))
	(entries '()))
    (do ((key 0 (+ key 1))
;;	 (low (* 6.875 (expt 2 .25)))
	 ) ; C = A * 2^(3/12)
	((= key 128) 
	 (let ((r (list "r" -1 -1 #f #f)))
	   (hash-set! *notes* "r" r)
	   (hash-set! *notes* -1 r))
	 #t)
      (set! entries (list-ref degrees (modulo key 12)))
      (do ((d entries (cdr d))
	   (k key)
	   (p (modulo key 12))
	   (k< (- key .5))
	   (k> (+ key .5))
;;	   (f (* low (expt 2 (/ key 12))))
;;	   (f> (* low (expt 2 (/ (+ key .5) 12))))
;;	   (f< (* low (expt 2 (/ (- key .5) 12))))
	   (f (ffi_keynum_to_hertz key))
	   (f> (ffi_keynum_to_hertz (+ key .5)))
	   (f< (ffi_keynum_to_hertz (- key .5)))
	   (o (inexact->exact (floor (/ key 12))))
	   (x #f)
	   (n #f)
	   (e #f))
	  ((null? d) #f)
	(unless (and (= k 0) (string=? (car d) "bs"))
	  (if (string=? (car d) "bs") (set! o (- o 1))
	      (if (string=? (car d) "cf") (set! o (+ o 1))))
	  ;; note entry: (<str> <key> <hz> <pc> <"oct">)
	  (set! x (list-ref octaves o))
	  (set! n (string-append (car d) x))
	  (set! e (list n k f p x))
	  (hash-set! *notes* n e)
	  (when (eq? d entries) ; add keynum->note hash
	    (hash-set! *notes* k e))

	  ;; add <quarter tone entries
	  (set! n (string-append (car d) "<" x))
	  (set! e (list n k< f< #f x))
	  (hash-set! *notes* n e)
	  (when (eq? d entries) ; add only one keynum->note hash
	    (hash-set! *notes*
		       ;; S7 doesnt hash floats
		       (+ (- key 1) 1000) ;  k<
		       e))
	  ;; add >quarter tone entries
	  (set! n (string-append (car d) ">" x))
	  (set! e (list n k> f> #f x))
	  (hash-set! *notes* n e)
	  (when (eq? d entries) ; add only one keynum->note hash
	    (hash-set! *notes*
		       ;; S7 doesnt hash floats
		       (+ key 1000) ; k>
		       e)
	    )
	  )))))

; (create-note-entries)

(define (string->note-entry str oct err)
  ;; note entry: (<str> <key> <hz> <pc> <"oct">)
  (let ((entry (hash-ref *notes* str )))
    (if (not entry)
	(if oct
	    (or (hash-ref *notes* (string-append str oct))
		(if err (error "~S is not a note or key" str) #f))
	    (and err (error "~S is not a note or key" str)))
	entry)))

;;(define (number->note-entry num err)
;;  (let ((fix (if (exact? num) num
;;		 (inexact->exact (round num)))))
;;    (hash-ref *notes* fix
;;		    (lambda () 
;;		      (and err (error "Not a note or key" num))))))

(define (number->note-entry num err)
  (if (exact? num) 
      (or (hash-ref *notes* num)
	  (if err (error "~S is not a note or key" num) #f))
      (let* ((int (inexact->exact (floor num)))
	     (rem (- num int)))
	(or (hash-ref *notes*
		      (if (< rem 0.333333333333333)
			  int
			  (if (< rem 0.666666666666667)
			      ;; S7 doesnt hash floats
			      (+ int 1000) ;(+ int .5)
			      (+ int 1))))
	    (if err (error "~S is not a note or key" num) #f)))))

(define (note-aux freq doct err?)
  ;; if doct (default octave string) we are parsing a note list
  (cond ((number? freq)
	 (number->note-entry freq err?))
	((symbol? freq)
	 (string->note-entry (symbol->string freq) doct err?))
	((string? freq)
	 (string->note-entry freq doct err?))
	((keyword? freq)
	 (string->note-entry (keyword->string freq) doct err?))	
	((and err?)
	 (error "~S is not a note or key" freq))
	(else
	 #f)))

(define (note freq)
  (if (list? freq)
      (let ((head (list #f)))
	(do ((tail freq (cdr tail))
	     (defo "4")
	     (this #f)
	     (resl head))
	    ((null? tail) (cdr head))
	  (if (list? (car tail)) ; allow sublists...
	      (begin
		(set! this (note (car tail) ))
		(set-cdr! resl (list this)))
	      (begin
		(set! this (note-aux (car tail) defo #t))
		;; note entry: (<str> <key> <hz> <pc> <"oct">)
		(let ((d (car (cddddr this))))
		  ;; dont reset default if a rest
		  (if d (set! defo d)))
		(set-cdr! resl (list (car this)))))
	  (set! resl (cdr resl))))
      (car (note-aux freq #f #t))))

;; (note '(c5 d e f))
;; (note '(60 50 70 32 ))
;; (note 60.45)
;; (note #:c5)
;; (note "cs6")
;; (note "cs")
;; (note '(c d e5 (f b) g))
;; (note (key 440))

;;(define-constant +loga+ (log 6.875))
;;(define-constant +log2+ (log 2))

(define (keynum freq)
  (cond ((list? freq)
	 (let ((head (list #f)))
	   (do ((tail freq (cdr tail))
		(defo "4")
		(this #f)
		(resl head))
	       ((null? tail) (cdr head))
	     (if (or (list? (car tail))
		     (number? (car tail)))
		 (begin
		   (set! this (keynum (car tail) ))
		   (set-cdr! resl (list this)))
		 (begin
		   (set! this (note-aux (car tail) defo #t))
		   ;; note entry: (<str> <key> <hz> <pc> <"oct">)
		   (set-cdr! resl (list (cadr this)))
		   (set! defo (car (cddddr this)))))
	     (set! resl (cdr resl)))))
	((number? freq)
	 (if (> freq 0)
	     (ffi_hertz_to_keynum freq)
	     -1))
	(else
	 (cadr (note-aux freq #f #t)))))

(define key keynum)

;; (keynum 440)
;; (keynum 'c4)
;; (keynum '(c4 d e2 f))
;; (keynum 'c)

(define (hertz freq)
  (cond ((list? freq)
	 (let ((head (list #f)))
	   (do ((tail freq (cdr tail))
		(defo "4")
		(this #f)
		(resl head))
	       ((null? tail) (cdr head))
	     (if (or (number? (car tail))
		     (list? (car tail)))
		 (begin
		   (set! this (hertz (car tail) ))
		   (set-cdr! resl (list this)))
		 (begin
		   (set! this (note-aux (car tail) defo #t))
		   (set-cdr! resl (list (caddr this)))
		   (set! defo (car (cddddr this)))))
	     (set! resl (cdr resl)))))
	((number? freq)
	 (ffi_keynum_to_hertz freq)
	 )
	(else
	 (caddr (note-aux freq #f #t)))))

;; remove eventually!
(define hz hertz)

;; (hertz 'c4)
;; (hertz 69)
;; (hertz '(c4 d e2 f g4 a))
;; (hertz '(20 60 87 33))
;; (hertz '(cs4 d e2 f g4 a))

(define (pitch-class x)
  ;; returns the pitch class (0-11) of a key, note or list of the same
  (if (number? x)
      (modulo (round x) 12)
      (if (pair? x)
	  (if (number? (car x))
	      (map pitch-class x)
	      (map pitch-class (keynum x)))
	  (pitch-class (keynum x)))))

;; remove eventually!
(define pc pitch-class)

;; (pitch-class 60)
;; (pitch-class 'cs4)
;; (pitch-class 'cs>4)
;; (pitch-class 'cs4)
;; (pitch-class '(d4 c f bf3))
;; (pitch-class '(50 30 12 22))

(define (rest? x)
  (or (eq? x 'r) (eq? x -1) (equal? x "r") (equal? x -1.0)))

(define (invert x)
  (cond ((number? x)
	 (if (< x 12)
	     (modulo (- 12 x) 12)
	     (error "~S is not a valid inversion" x)))
	((pair? x)
	 (let ((invkeys
		(lambda (keys) 
		  ;; invert list of keys around first key in list
		  (let* ((orig (car keys)) 
			 (head (list orig)))
		    (do ((tail (cdr keys) (cdr tail))
			 (last head))
			((null? tail)
			 head)
		      (set-cdr! last (list (- orig (- (car tail) orig))))
		      (set! last (cdr last)))))))
	   ;; check list for pcs, keynums or notes
	   (if (number? (car x))
	       (if (< (car x) 12)
		   (map (lambda (z) (modulo (- 12 z) 12)) x)
		   (invkeys x))
	       (note (invkeys (keynum x))))))
	(else
	 (error "~S is not a valid inversion" x))))

; (invert '(60 62 64 ))
; (invert  7)
; (invert '(0 7 2 1))
; (invert 60) ; err!
; (invert '(bf3 df4 f))

(define (transpose x y)
  (cond ((number? x)
	 (if (number? y)
	     (if (and (< x 12) (< y 12))
		 (modulo (+ x y) 12)
		 (+ x y))
	     (note (+ x (keynum y)))))
	((pair? x)
	 (let ((transpkeys
		(lambda (keys orig) 
		  ;; transpose a list of pcs keys or notes
		  (let ((head (list #f)))
		    (do ((tail keys (cdr tail))
			 (last head))
			((null? tail)
			 (cdr head))
		      (set-cdr! last (list (+ orig (car tail))))
		      (set! last (cdr last))))))
	       (orig (if (number? y) y (keynum y))))
	   ;; check first in list for pc
	   (if (number? (car x))
	       (if (and (< (car x) 12) (< orig 12))
		   (map (lambda (z) (modulo (+ z orig) 12)) x)
		   (transpkeys x orig))
	       (note (transpkeys (keynum x) orig)))))
	((or (symbol? x) (string? x))
	 (note (+ (keynum x) y))
	 )
	(else
	 (error "~S is not a valid transposion" x))))

; (transpose 7 7)
; (transpose '(0 1 2 3 4 5 6 7 8 9 10 11) 7)
; (invert '(0 1 2 3 4 5 6 7 8 9 10 11))
; (transpose '(0 3 7) 'c4)
; (transpose '(0 3 7) 11)
; (transpose '(c4 e g) 12)

;(define (matrix row)
;  (map (lambda (i) (transpose row i))
;       (invert row)))
;
;(matrix '(0 1 2 3 4 5 6 7 8 9 10 11))

(define retrograde reverse)

(define (scale-order scale . mode)
  (if (null? mode) (set! mode 1)
      (set! mode (car mode)))
  (if (eqv? mode 1)
      (sort scale (lambda (a b)
		    (if (number? a)
			(if (number? b)
			    (< a b)
			    (< a (keynum b)))
			(if (number? b)
			    (< (keynum a) b)
			    (< (keynum a) (keynum b))))))
  (if (eqv? mode -1)
      (sort scale (lambda (a b)
		    (if (number? a)
			(if (number? b)
			    (> a b)
			    (> a (keynum b)))
			(if (number? b)
			    (> (keynum a) b)
			    (> (keynum a) (keynum b))))))
      (if (eqv? mode 0)
	  (shuffle scale)
	  (error "~S is not a valid mode" mode)))))

;;
;; rhythm
;;

(define *tempo* 60.0)

(define *beat* .25)

(define *rhythms* (make-equal-hash-table))

;; create rhythm tokens
(let ((toks '((1/64 "x" 0)
	      (1/32 "t" 1)
	      (1/16 "s" 2)
	      (1/8  "e" 3)
	      (1/4  "q" 4)
	      (1/2  "h" 5)
	      (1    "w" 6)))
      (rsym (lambda (pre raw post)
	      (string-append pre raw post)))
      (dots '(("." 1/2) (".." 3/4) ("..." 7/8) ("...." 15/16)
	      ("....." 31/32) ("......" 63/64)))
      (addryth (lambda (str val)
		 ;;(set! foos (cons str (cons (string->symbol str) foos)))
		 (hash-set! *rhythms* str val)
		 (hash-set! *rhythms* (string->symbol str) val))))
  (for-each 
   (lambda (e)
     (let ((rat (car e))
	   (sym (cadr e))
	   (dot (caddr e)))
       (addryth sym rat)
       (addryth (rsym "t" sym "") (* rat 2/3))
       (do ((i 0 (+ i 1))
	    (x #f)
	    (d #f)
	    (r #f))
	   ((not (< i dot)) #f)
	 (set! x (list-ref dots i))
	 (set! d (car x))
	 (set! r (cadr x))
	 (addryth (rsym "" sym d) (+ rat (* rat r)))
	 (addryth (rsym "t" sym d) (+ (* rat 2/3) (* rat 2/3 r))))))
   toks))

(define (rhythm val . args)
  (let ((tempo *tempo*)
	(beat *beat*))
    (if (pair? args)
	(begin (set! tempo (car args))
	       (if (pair? (cdr args))
		   (set! beat (cadr args)))))
    (cond ((number? val)
	   (* (/ val beat) (/ 60 tempo)))
	  ((or (symbol? val)
	       (string? val))
	   (let ((x (hash-ref *rhythms* val)))
	     (if x
		 (* (/ x beat) (/ 60 tempo))
		 (rhythm-expr val tempo beat))))
	  ((list? val)
	   (map (lambda (x) (rhythm x tempo beat)) val))
	  (else
	   (error "~S is not a rhythm" val)))))

(define (rhythm-expr expr tempo beat)
  (let ((ryth (if (symbol? expr) (symbol->string expr) expr))
	(next-token-start
	 (lambda (str lb len)
	   (do ((ops '(#\+ #\- #\* #\/ ))
		(i lb (+ i 1)))
	       ((or (= i len) (member (string-ref str i) ops))
		i)))))
    (let* ((len (string-length ryth))
	   (lb 0)
	   (ub (next-token-start ryth lb len)))
      ;;(if (not (< lb ub)) (error "Not a rhythm" ryth))
      (if (not (< lb ub)) 
	  (if (and (> len 1) (char=? (string-ref ryth 0) #\-))
             ;(let ((val (or (hash-ref *rhythms* (substring ryth 1 len))
             ;  (error "Not a rhythm" ryth))))
             ;  (* (/ (- val) beat) (/ 60 tempo)))
	      (* -1  (rhythm-expr (substring ryth 1) tempo beat))
	      (error "~S is not a rhythm" ryth) )
	  (do ((num (or (hash-ref *rhythms* (substring ryth lb ub))
			(error "~S is not a rhythm" expr)
			))
	       (val #f)
	       (tok #f)
	       (op #f)
	       (i 0 (+ i 1)))
	      ((not (< ub len))
	       ;; update hash table for faster lookup next time
	       (hash-set! *rhythms* ryth num)
	       (if (not (eqv? ryth expr))
		   (hash-set! *rhythms* expr num))
	       (* num (/ 1 beat) (/ 60 tempo)))
	    (set! op (string-ref ryth ub))
	    (set! lb (+ ub 1))
	    (set! ub (next-token-start ryth lb len))
	    (if (< lb ub)
		(set! tok (substring ryth lb ub))
		(error "~S is not a rhythm" ryth))
	    (cond ((char=? op #\+)
		   (set! val (or (hash-ref *rhythms* tok)
				 (error "~S is not a rhythm" ryth)))
		   (set! num (+ num val)))
		  ((char=? op #\-)
		   (set! val (or (hash-ref *rhythms* tok)
				 (error "~S is not a rhythm" ryth)))
		   (set! num (- num val)))
		  ((char=? op #\*)
		   (set! val (string->number tok))
		   (set! num (* num val)))
		  ((char=? op #\/)
		   (set! val (string->number tok))
		   (set! num (/ num val)))
		  (else
		   (error "~S is not a rhythm" ryth))))))))

; (rhythm 'q-x)
; (rhythm 'w.*4)
; (rhythm 'foo)

(define (in-tempo n tempo)
  (times n (/ 60.0 tempo)))

;;
;; promise (delay)
;;

(define-macro (promise expr) `(lambda () ,expr))

;;
;; file system and pathnames
;;

(define (home-directory)
  (ffi_user_home_directory))

(define (temp-directory)
  (ffi_temp_directory))

(define (pwd )
  (ffi_current_directory ))

(define (chdir str)
  (ffi_set_current_directory str))

;;(define (directory str . rec)
;;  (let ((res (ffi_directory str (and (pair? rec) (car rec)))))
;;    (if (string=? res "") (list) 
;;	(read-from-string res))))

(define (directory str . rec)
  (ffi_directory str (and (pair? rec) (car rec))))

(define (make-pathname . args)
  (with-optkeys (args directory name type defaults)
    (let ((d #f) 
	  (n #f)
	  (t #f)
	  (p #f))
      (if (not defaults) 
	  (set! defaults "")
	  (if (not (string? defaults))
	      (error "~S is not a defaults string" defaults)))
      (set! d (or directory (ffi_pathname_directory defaults)))
      (set! n (or name (ffi_pathname_name defaults)))
      (set! t (or type (ffi_pathname_type defaults)))
      (if (string? d)
	  (let ((l (string-length d)))
	    (if (> l 0)
		(if (not (char=? (string-ref d (- l 1)) #\/))
		    (error "directory ~S does not end with a delimiter" 
			   directory)))
	    (set! p d))
	  (error "directory ~S is not a string" directory))
      (if (string? n)
	  (set! p (string-append p n))
	  (error "file name ~S is not a string" name))
      (if (string? t)
	  (if (not (equal? t ""))
	      (set! p (string-append p "." t)))
	  (error "file type ~S is not a string" type))
      p)))
	  
(define (pathname-name path)
  (if (string? path)
      (let ((p (ffi_pathname_name path)))
	(if (string=? p "") #f p))
      (error "pathname ~S is not a string" path)))

(define (pathname-type path)
  (if (string? path)
      (let ((p (ffi_pathname_type path)))
	(if (string=? p "") #f p))
      (error "pathname ~S is not a string" path)))

(define (pathname-directory path)
  (if (string? path)
      (let ((p (ffi_pathname_directory path)))
	(if (string=? p "") #f p))
      (error "pathname ~S is not a string" path)))

(define (full-pathname path)
  (if (string? path)
      (ffi_full_pathname path) 
      (error "pathname ~S is not a string" path)))

(define (pathname-exists? path)
  (if (string? path)
      (ffi_pathname_exists_p path)
      (error "pathname ~S is not a string" path)))

(define (pathname-directory? path)
  (if (string? path)
      (ffi_pathname_directory_p path)
      (error "pathname ~S is not a string" path)))

(define (pathname->key path)
  (ffi_pathname_to_key path))

;; file-version

(define *versions* (make-equal-hash-table))

; (file-version "~/Work/test.mid" )
; (file-version "test.mid" 10)
; (file-version "test.mid" )

(define (file-version file . args)
  (with-optkeys (args (version #t) (nooverwrite #f))
    (let ((ver version))
      (if (not ver)
          file
          (let* ((nam (pathname-name file))
                 (ext (pathname-type file))
                 (key (string-append nam "." ext))
                 (num (hash-ref *versions* key)) ; num to use or #f
                 )
            (cond ((and (integer? ver) (>= ver 0))
                   (set! num ver) )
                  ((eq? ver #t)
                   (if (not num) (set! num 1)))
                  (else
                   (error "versioning value not #t, #f or integer: ~S" ver)))
            (if nooverwrite
                (set! num (ffi_insure_new_file_version file num)))
            ;; store next version number for file
            (hash-set! *versions* key (+ num 1))
            (make-pathname :name (string-append nam "-" (number->string num))
                           :defaults file))))))

(define *colors*
  '("black"
    "white"
    "blue"
    "grey"
    "green"
    "red"
    "yellow"
    "aliceblue"
    "antiquewhite"
    "aqua"
    "aquamarine"
    "azure"
    "beige"
    "bisque"
    "blanchedalmond"
    "blueviolet"
    "brown"
    "burlywood"
    "cadetblue"
    "chartreuse"
    "chocolate"
    "coral"
    "cornflowerblue"
    "cornsilk"
    "crimson"
    "cyan"
    "darkblue"
    "darkcyan"
    "darkgoldenrod"
    "darkgrey"
    "darkgreen"
    "darkkhaki"
    "darkmagenta"
    "darkolivegreen"
    "darkorange"
    "darkorchid"
    "darkred"
    "darksalmon"
    "darkseagreen"
    "darkslateblue"
    "darkslategrey"
    "darkturquoise"
    "darkviolet"
    "deeppink"
    "deepskyblue"
    "dimgrey"
    "dodgerblue"
    "firebrick"
    "floralwhite"
    "forestgreen"
    "fuchsia"
    "gainsboro"
    "gold"
    "goldenrod"
    "greenyellow"
    "honeydew"
    "hotpink"
    "indianred"
    "indigo"
    "ivory"
    "khaki"
    "lavender"
    "lavenderblush"
    "lemonchiffon"
    "lightblue"
    "lightcoral"
    "lightcyan"
    "lightgoldenrodyellow"
    "lightgreen"
    "lightgrey"
    "lightpink"
    "lightsalmon"
    "lightseagreen"
    "lightskyblue"
    "lightslategrey"
    "lightsteelblue"
    "lightyellow"
    "lime"
    "limegreen"
    "linen"
    "magenta"
    "maroon"
    "mediumaquamarine"
    "mediumblue"
    "mediumorchid"
    "mediumpurple"
    "mediumseagreen"
    "mediumslateblue"
    "mediumspringgreen"
    "mediumturquoise"
    "mediumvioletred"
    "midnightblue"
    "mintcream"
    "mistyrose"
    "navajowhite"
    "navy"
    "oldlace"
    "olive"
    "olivedrab"
    "orange"
    "orangered"
    "orchid"
    "palegoldenrod"
    "palegreen"
    "paleturquoise"
    "palevioletred"
    "papayawhip"
    "peachpuff"
    "peru"
    "pink"
    "plum"
    "powderblue"
    "purple"
    "rosybrown"
    "royalblue"
    "saddlebrown"
    "salmon"
    "sandybrown"
    "seagreen"
    "seashell"
    "sienna"
    "silver"
    "skyblue"
    "slateblue"
    "slategrey"
    "snow"
    "springgreen"
    "steelblue"
    "tan"
    "teal"
    "thistle"
    "tomato"
    "turquoise"
    "violet"
    "wheat"
    "whitesmoke"
    "yellowgreen"))

;
;; Sound database support for using with vkey and sc:vkey
;

; sound descriptors are entries in a sound db 

(define (sd-key sd) (car sd))
(define (sd-file sd) (cadr sd))
(define (sd-buffer sd) (caddr sd))
(define (sd-duration sd) (cadddr sd))
(define (sd-channels sd) (cadddr (cdr sd)))
(define (sd-amplitude sd)
  ;; amp calc delayed until user accesses the value
  (let ((amp (cadddr (cddr sd))))
    (or amp
        (do ((info (mus-sound-maxamp (sd-file sd)) (cddr info))
             (maxa 0.0))
            ((null? info) 
             (set-car! (cdr (cddddr sd)) maxa)
             maxa)
          (set! maxa (max maxa (cadr info)))))))

(define* (sound-db dir (decode pathname->key) (full #t) assoc)
  ;; create a sound db suitable for sdey or sc:vkey
  ;; db is a vector of (keynum pathname buffer-number duration)
  ;; buffer numbers are initially #f
  (let* ((filenames (if (pair? dir) dir (directory dir)))
         (len (length filenames))
         (result (if (> len 0) (make-vector len)
                     (error "sound-db: no matching files for ~S" dir))))
    (if (eq? assoc #t) (set! assoc "file"))
    (let recur ((i 0)
                (files filenames))
      (if (null? files)
          (sort! result (lambda (x y) (< (car x) (car y))))
          ;; sound descriptor
          (let ((sd (list (if assoc (format #f "~A~D" assoc (+ i 1))
                              (decode (car files)))
                          (car files)           ; pathname
                          #f                    ; sc buffer(s)
                          (if full (mus-sound-duration (car files)) #f)
                          (if full (mus-sound-chans (car files)) #f)
                          #f)))                 ; maxamp   
            (vector-set! result i sd )
            (recur (1+ i) (cdr files)))))
    (format #t "sound-db: ~S in ~D files, key ~A to ~A~%"
            dir len (car (result 0)) (car (result (- len 1))))
    result))

(define* (closest-index item vec (test <) (key (lambda (x) x)))
  ;; finds the closest index in a vector using a binary search. used
  ;; by vkey to determine closest sound db entry to a given keynumber.
  (let search ((start 0)
               (stop (- (vector-length vec) 1)))
    (if (< stop start)
        ;; end of binary search - not found
        ;; determine which items is closest
        (cond
         ((< stop 0) start)
         ((>= start (vector-length vec)) stop)
         (else
          (let ((stopdist (abs (- (key (vector-ref vec stop)) item)))
                (startdist (abs (- (key (vector-ref vec start)) item))))
            (if (< stopdist startdist)
                stop start))))
        ;; continue binary search
        (let* ((midpoint (quotient (+ start stop) 2))
               (mid-value (key (vector-ref vec midpoint))))
          (cond ((test item mid-value)
                 (search start (- midpoint 1)))
                ((test mid-value item)
                 (search (+ midpoint 1) stop))
                (else midpoint))))))

; (midifile-import "foo.mid" 1 "key")
; (midifile-import "foo.mid" 1 '("key" "time"))
; (midifile-import "foo.mid" 1 '("key" "time" "press"))
; (midifile-import "foo.mid" 1 '(chan ctrl1 ctrl2))
; (midifile-import "foo.mid" 1 '(ctrl2))
; (midifile-import "foo.mid" 1 '(:rhythm :dur :vel))
; (midifile-import "foo.mid" 1 '(prog "chan"))

(define midi-values  
  ;; SEE:  Enumerations.h
  '(("time"   1 ) 
    ("delta"  2 )
    ("op"     3 )
    ("chan"   4 ) 
    ("rhythm" 5 )
    ("dur"    6 )
    ("key"    7 )
    ("amp"    8 )
    ("vel"    9 )
    ("touch"  10 )
    ("ctrl1"  11 )
    ("ctrl2"  12 )
    ("prog"   13 )
    ("press"  14 )
    ("bend"   15 )
    ("seqnum"     16)
    ("text"       17)
    ("chanpre"    18)
    ("tempo"      19)
    ("timesig"    20)
    ("keysig"     21)
    ))

(define (midifile-import file track values)
  (unless (file-exists? file)
    (error "file does not exist: ~S" file))
  (define (getname x)
    (cond ((string? x) x)
          ((keyword? x) (keyword->string x))
          ((symbol? x) (symbol->string x))
          (else (error "not a midi value: ~S" x))))
  (define (getmidivalue x l)
    (let ((e (or (assoc (getname x) l)
                 (error "not a midi value: ~S" x))))
      (second e)))
  (unless (and (integer? track) (>= track 0))
    (error "not a track number: ~S" track))
  (if (pair? values)
      (if (pair? (car values))
          (set! values (map (lambda (y) 
                              (or (pair? y) (error "not a list of midi values: ~S" y))
                              (map (lambda (x) (getmidivalue x midi-values)) y))
                            values))
          (set! values (map (lambda (x) (getmidivalue x midi-values)) values)))
      (set! values (list (getmidivalue values midi-values))))
  (ffi_midifile_import file track values)
  )

; (midifile-import "/Users/hkt/incline/zincline-1.mid" 0 "op")

(define (midifile-header file)
  (ffi_midifile_header file #f))

