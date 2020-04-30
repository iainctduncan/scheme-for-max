;;; **********************************************************************
;;; Copyright 1999-2013 Rick Taube.  All rights reserved.
;;; Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
;;; Public License, which says that non-commercial users may share and
;;; modify this code but must give credit and share improvements. For
;;; complete terms please read the text of the full license available at
;;; this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html
;;; **********************************************************************

(define (harmonics h1 h2 . args)
  (with-optkeys (args (fund 1) invert order keys)
    ;; calculate overtones from h1 to h2. if fundamental is 1 (the
    ;; default) then freq ratios are returned.
    (let* ((freq fund)
           (head (list #f))
           (tail head)
           (undertones #f)
           (spec '()))
      (cond ((and (integer? h1) (integer? h2) (< 0 h1 h2))
             #f)
            ((and (integer? h2) (integer? h2) (> 0 h1 h2))
             (set! undertones #t)
             (set! h1 (abs h1))
             (set! h2 (abs h2)))
            (else
             (error "illegal harmonic values: ~S ~S." h1 h2)
             ))
      (set! spec
            (if invert
                (do ((h h2 (- h 1))
                     (f #f))
                    ((< h h1) (cdr head) )
                  (if undertones
                      (set! f (/ freq (/ h2 h )))
                      (set! f (* freq (/ h2 h))))
                  (set-cdr! tail (list f))
                  (set! tail (cdr tail)))

                (do ((h h1 (+ h 1))
                     (f #f))
                    ((> h h2) (cdr head) )
                  (if undertones
                      (set! f (/ freq (/ h h1)))
                      (set! f (* freq (/ h h1))))
                  (set-cdr! tail (list f))
                  (set! tail (cdr tail)))
                ))

      (case order
        (( #f ) spec)
        (( 1 )
         (if undertones (set! spec (reverse! spec))))
        (( -1 )
         (if (not undertones) (set! spec (reverse! spec))))
        (( 0 ) (set! spec (shuffle spec)))
        (else
         (error "harmonics order not 1 0 or -1" order)))
      (if keys
          (do ((tail spec (cdr tail)))
              ((null? tail)
               spec)
            (if (= fund 1)
                (set-car! tail (* (log2 (car tail)) 12))
                (set-car! tail (ffi_hertz_to_keynum (car tail)))
            ))
          spec))))

(define-record spectrum time size freqs amps)

;;(define-record-printer (spectrum obj port)
;;  (fprintf port "#<spectrum ~S>"
;;	   (spectrum-freqs obj)
;;	   ;;(spectrum-amps obj)
;;	   ))

;(define (make-spectrum freqs . args)
;  (let ((amps (if (null? args) #f (list->vector (car args))))
;	(size (length freqs)))
;    (make-spektrum #f size (list->vector freqs)
;		   amps)))
;(define spectrum-size spektrum-size)
;(define spectrum-freqs spektrum-freqs)
;(define spectrum-amps spektrum-amps)
;(define spectrum-time spektrum-time)

(define (spectrum-copy spectrum)
  (make-spectrum (spectrum-time spectrum)
		 (spectrum-size spectrum)
		 (append (spectrum-freqs spectrum) (list))
		 (append (spectrum-amps spectrum) (list))))

(define (spectrum-pairs spectrum)
  (let* ((head (list #f))
	 (tail head)
	 (freqs (spectrum-freqs spectrum))
	 (ampls (spectrum-amps spectrum))
	 (size (spectrum-size spectrum)))
    (do ((i 0 (+ i 1)))
	((= i size) (cdr head))
      (set-cdr! tail (list (list-ref freqs i) 
			   (if ampls (list-ref ampls i) 0.0)))
      (set! tail (cddr tail)))))

(define (spectrum-minfreq spectrum)
  (car (spectrum-freqs spectrum)))

(define (spectrum-maxfreq spectrum)
  (list-ref (spectrum-freqs spectrum)
	    (- (spectrum-size spectrum) 1)))

(define (spectrum-minamp spectrum)
  (let ((amp #f)
	(amps (spectrum-amps spectrum)))
    (if (not amps) 0.0
	(begin (for-each (lambda (a)
			   (if (or (not amp) (< a amp)) (set! amp a)))
			 amps)
	       amp))))

(define (spectrum-maxamp spectrum)
  (let ((amp 0.0)
	(amps (spectrum-amps spectrum)))
    (if (not amps) amp
	(begin (for-each (lambda (a) (if (> a amp) (set! amp a)))
			 amps)
	       amp))))

(define (spectrum->keys spectrum order thresh quant unique
			mink maxk)
  (if (eqv? unique #t) (set! unique 1))
  (let* ((head (list #f))
	 (tail head)
	 (sort? #f)
	 (freqs (spectrum-freqs spectrum))
	 (ampls (spectrum-amps spectrum)))
    (letrec ((getkey 
	      (lambda (h)
		(let ((k (if quant (quantize (keynum h) quant) (keynum h))))
		  (cond ((and mink (< k mink))
			 (do ()
			     ((>= k mink)
			      (set! sort? #t) k)
			   (set! k (+ k 12))))
			((and maxk (> k maxk))
			 (do () 
			     ((<= k maxk) 
			      (set! sort? #t) k)
			   (set! k (- k 12))))
			(else k)))))
	     (add (lambda (k)
		    (set-cdr! tail (list k))
		    (set! tail (cdr tail))))
	     (addkey (lambda (h) (add (getkey h))))
	     (adduniquekey (lambda (h) 
			     (let ((k (getkey h)))
			       (if (not (member k (cdr head)))
				   (add k))))))
      (if (or (not thresh) (not ampls))
	  (if unique
	      (for-each adduniquekey freqs)
	      (for-each addkey freqs))
	  (if unique
	      (for-each (lambda (a b)
			  (if (>= b thresh) (adduniquekey a)))
			freqs ampls)
	      (for-each (lambda (a b)
			  (if (>= b thresh) (addkey a)))
			freqs ampls)))
      (let ((keys (cdr head)))
	(cond ((eqv? order 0)
	       (shuffle! keys))
	      ((eqv? order -1)
	       (if sort? (sort! keys >)
		   (reverse! keys)))
	      (sort?
	       (sort! keys <))
	      ((eqv? order 1)
	       keys)
	      (else 
	       (error "mode ~S not -1 0 or 1" mode))))
      )))


(define (spectrum-keys spectrum . args)
  (with-optkeys (args (order 1) (thresh #f) (quant #f) (unique #f)
		      (min #f) (max #f) ;(fit #f)
		      )
;    (let ((mink #f) (maxk #f))
;      (cond ((pair? fit)
;	     (set! mink (car fit))
;	     (if (pair? (cdr fit))
;		 (set! maxk (cadr fit))))
;	    ((number? fit) (set! mink fit)))
      (spectrum->keys spectrum order thresh quant unique
		      min max)
;      )
    ))

; (define aaa (fm-spectrum1 100 1.4 3))
; (spectrum-time aaa)
; (spectrum-size aaa)
; (spectrum-freqs aaa)
; (spectrum-amps aaa)
; (spectrum-pairs aaa)
; (spectrum-maxfreq aaa)
; (spectrum-minfreq aaa)
; (spectrum-maxamp aaa)
; (spectrum-minamp aaa)
; (spectrum-keys aaa)

; (spectrum->keys aaa 1 .5 #f #f)
; (spectrum->keys aaa 1 #f #f #f)
; (spectrum->keys aaa -1 #f #f #f)
; (spectrum-keys aaa 1 .5)
; (spectrum-keys aaa -1 .5)
; (spectrum-keys aaa 0 .5)
; (spectrum-keys aaa 0 int: #t unique: #t)

;;;--------------------------------------------------------------------

(define (fm-spectrum carrier mratio index)
  (let ((mfreq (* carrier mratio))
        (nsides (+ (inexact->exact (round index)) 1) )
        (spectrum '()))
    ;; spectrum is a list of sidebands, each sideband is (freq amp)
    (let* ((head (list #f))
	   (tail head))
      (do ((k (- nsides) (+ k 1))
	   (f #f)
	   (a #f))
	  ((> k nsides)
	   (set! spectrum (cdr head)))
	(set! f (+ carrier (* mfreq k)))
	(set! a (ffi_bes_jn k index))
	(if (not (or (= a 0.0) (= f 0.0)))
	    (begin
	      (set-cdr! tail (list (list f a)))
	      (set! tail (cdr tail))))))
    ;; fold negative freqs into positive side, combine sideband amps
    (do ((neg #f)
	 (pos #f))
	((not (negative? (caar spectrum))) #f)
      (set! neg (car spectrum)) ; pop
      (set! spectrum (cdr spectrum))
      (set! pos (abs (first neg)))
      ;; wrap around to positive freq, invert phase
      (set-car! neg pos)
      (set-car! (cdr neg) (- (second neg)))
      ;; combine with existing sideband or insert at freq position.
      (let ((side (assoc (first neg) spectrum)))
	(if side
            (set-car! (cdr side)
                      (+ (cadr side) 
                         (second neg)))
            (let ((p (do ((i (- (length spectrum) 1) (- i 1))
                          (f #f)
                          (x #f))
                         ((or f (< i 0)) f)
                       (set! x (car (list-ref spectrum i)))
                       (if (< x pos) (set! f i)))))
              (if (not p)
		  (set! spectrum (cons neg spectrum)  )
		  (let ((tail (list-tail spectrum p)))
		    (set! neg (list neg))
		    (set-cdr! neg (cdr tail))
		    (set-cdr! tail neg)))))))
    ;; create the lists of freqs and amps, reusing cons cells in
    ;; spectrum, ie ((a 1) (b 2) (c 3)) => (a b c) (1 2 3). outer list
    ;; reused as freqs list, cdrs of inner lists nconced together for
    ;; amps list.
    (let* ((freqs spectrum)
	   (ampls (car spectrum))) ; (a 1)
      (do ((tail spectrum (cdr tail))
	   (size 0 (+ size 1))
	   (ampl ampls))
	  ((null? tail)
	   (make-spectrum #f size freqs (cdr ampls)))
	(let* ((entry (car tail))
	       (f (car entry)))
	  ;; no negative amps
	  (set-car! (cdr entry) (abs (cadr entry)))
	  (set-cdr! ampl (cdr entry))
	  (set! ampl (cdr ampl))
	  (set-car! tail f))))))

; (fm-spectrum1 100 1.4 3)
; (fm-spectrum 100 1.4 3 )
; (fm-spectrum 100 1.4 3 :amplitudes #f)
; (fm-spectrum 100 1.4 3 :spectrum :hertz)
; (fm-spectrum 100 1.4 3 :spectrum :hertz :amplitudes #t)

;         50
;(10 30 40 50)
;         44
;(10 30 40 50)
;             60
;(10 30 40 50)

(define (spectrum-sort! spec)
  (let ((entries (map (lambda (a b) (list a b))
		      (spectrum-freqs spec)
		      (spectrum-amps spec))))
    (set! entries (sort! entries (lambda (a b) (< (car a) (car b)))))
    ;; create the lists of freqs and amps reusing cons cells in
    ;; entries, ie ((a 1) (b 2) (c 3)) => (a b c) (1 2 3). outer list
    ;; reused as freqs list, cdrs of inner lists nconced together for
    ;; amps list.
    (let ((freqs entries)
	  (ampls (car entries))) ; (a 1)
      (do ((tail entries (cdr tail))
	   (ampl ampls))
	  ((null? tail)
	   (spectrum-freqs-set! spec freqs)
	   (spectrum-amps-set! spec (cdr ampls))
	   spec)
	(let* ((entry (car tail))
	       (f (car entry)))
	  (set-cdr! ampl (cdr entry))
	  (set! ampl (cdr ampl))
	  (set-car! tail f))))))

(define (spectrum-add! spec freq amp)
  (do ((freqs (spectrum-freqs spec) (cdr freqs))
       (amps (spectrum-amps spec) (cdr amps))
       (lastf '() freqs)
       (lasta '() amps) )
      ;; search freqs for insertion point, ie point where car is equal
      ;; or greater then freq
      ((or (null? freqs) (>= (car freqs) freq ))
       (cond ((null? freqs)
	      ;; empty freqs or freq is higher than any in list
	      (if (null? lastf)
		  (begin
		    (spectrum-freqs-set! spec (list freq))
		    (spectrum-amps-set! spec (list amp))
		    (spectrum-size-set! spec 1))
		  (begin
		    (set-cdr! lastf (list freq))
		    (set-cdr! lasta (list amp))
		    (spectrum-size-set! spec (+ (spectrum-size spec)
						1)))))
	     ((= (car freqs) freq) 
	      ;; freq already present just update amp
	      (set-car! amps (+ (car amps) amp)))
	     ((null? lastf)
	      ;; freq is lower than any in list
	      (spectrum-freqs-set! spec (cons freq freqs))
	      (spectrum-amps-set! spec (cons amp amps))
	      (spectrum-size-set! spec (+ (spectrum-size spec) 1)) )
	     (else   ;; insert before car of freqs
	      (let ((f (list freq))
		    (a (list amp)))
		(set-cdr! f freqs)
		(set-cdr! lastf f)
		(set-cdr! a amps)
		(set-cdr! lasta a)
		(spectrum-size-set! spec
				    (+ (spectrum-size spec) 1)))))
       spec)))

; (define aaa (make-spectrum 0 1 (list 100) (list .1)))
; (print aaa)
; (spectrum-add! aaa 200 .2)
; (spectrum-add! aaa 10 .1)
; (spectrum-add! aaa 50 .5)
; (spectrum-add! aaa 300 .3)
; (spectrum-add! aaa 150 .15)
; (spectrum-add! aaa 1999 .19)

(define (rm-spectrum spec1 spec2 )
  (let ((getfreqs 
	 (lambda (s)
	   (cond ((spectrum? s) (spectrum-freqs s))
		 ((pair? s) s)
		 (else s))))
	(getamps
	 (lambda (s)	     
	   (cond ((spectrum? s) (spectrum-amps s))
		 ((pair? s) 0.0)
		 (else 0.0))))
	(sums-and-diffs 
	 (lambda (s f1 a1 f2 a2)
	   (if (not (= f1 f2) )
	       (begin (spectrum-add! s (+ f1 f2) 
				     0.0)
		      (spectrum-add! s (abs (- f1 f2))
				     0.0))))))
    (let ((freq1 (getfreqs spec1))
	  (freq2 (getfreqs spec2))
	  (amps1 (getamps spec1))
	  (amps2 (getamps spec2))
	  (spect (make-spectrum #f 0 (list) (list))))
      (if (pair? freq1)
	  (if (pair? freq2)
	      (do ((l2 freq2 (cdr l2)))
		  ((null? l2) #f)
		(do ((l1 freq1 (cdr l1)))
		    ((null? l1) #f)
		  (sums-and-diffs spect (car l1) 0.0
				  (car l2) 0.0)))
	      (do ((l1 freq1 (cdr l1)))
		  ((null? l1) #f)
		(sums-and-diffs spect (car l1) 0.0 freq2 0.0)))
	  (if (pair? freq2)
	      (do ((s2 freq2 (cdr s2)))
		  ((null? s2) #f)
		(sums-and-diffs spect freq1 0.0 (car s2) 0.0))
	      (sums-and-diffs spect freq1 0.0 freq2 0.0)))
      spect)))

(define (spectrum-flip! spec)
  (do ((maxf (spectrum-maxfreq spec))
       (minf (spectrum-minfreq spec))
       (tail (spectrum-freqs spec) (cdr tail))
       (flip (list)))
      ((null? tail)
       (spectrum-freqs-set! spec flip)
       spec)
    ;;(set! r (cons (* maxf (/ (car l) ) minf) r))
    (set! flip (cons (* minf (/ maxf (car tail)))
		     flip))))

;(define (spectrum-invert! spec)
;  (do ((minf (spectrum-minfreq spec))
;       ;; reverse the list so that inverted values will still be in
;       ;;  ascending order
;       (tail (reverse! (spectrum-freqs spec)) (cdr tail))
;       )
;      ((null? (cdr tail)) ; skip minf element
;       ;;(spectrum-freqs-set! spec tail)
;       spec)
;    (set-car! tail (* minf (/ minf (car tail))))))

(define (spectrum-invert! spec . point)
  (do ((freq (if (null? point)
                 (spectrum-minfreq spec)
                 (car point)))
       ;; reverse the list so that inverted values will still be in
       ;;  ascending order
       (tail (reverse! (spectrum-freqs spec)) (cdr tail))
       )
      ((null? tail)
       spec)
    (set-car! tail (* freq (/ freq (car tail))))))

; (define aaa (make-spectrum #f 5 (hertz '(a3 e4 b4 ds5 fs5)) '(0 0 0 0 0)))
; (note (keynum (spectrum-freqs aaa)))
; (spectrum-invert! aaa)
; (reverse (note (keynum (spectrum-freqs aaa))))

; (define aaa (make-spectrum #f 5 (hertz '(a3 e4 b4 ds5 fs5)) '(0 0 0 0 0)))
; (note (keynum (spectrum-freqs aaa)))
; (spectrum-invert! aaa (hertz 'e4))
; (reverse (note (keynum (spectrum-freqs aaa))))

; (define foo (fm-spectrum (hertz 'c4) 1.4983070768767 2.9212598425197 ))
; (note (spectrum-keys foo))
; (spectrum-invert! foo)
; (note (spectrum-keys foo :order -1))

(define* (spectrum-rescale! spec (min #t) (max #t) )
  (let ((data (spectrum-freqs spec))
        (newmin min)
        (newmax max)
        (oldmin (spectrum-minfreq spec))
        (oldmax (spectrum-maxfreq spec)))
    (if (eq? newmin #t) (set! newmin (spectrum-minfreq spec)))
    (if (eq? newmax #t) (set! newmax (spectrum-maxfreq spec)))
    (if (< newmin newmax)
        (do ((tail data (cdr tail)))
            ((null? tail)
             spec)
          (set-car! tail (rescale (car tail) oldmin oldmax newmin newmax)))
        (error "minimum ~S not less than maximum ~S" newmin newmax))))

;; interp key note every

(define (spectrum-modify! spec mode . args)
  (let ((scaling #f) 
	(modifier #f)
	(modified #f)
	(minimum #f)
	(maximum #f))
    (if (null? args)
	(error "missing scaler, envelope or x y values")
	(if (null? (cdr args))
	    (set! args (if (or (pair? (car args))
			       (number? (car args)))
			   (car args)
			   (error "~S is not a number or pair"
				  (car args))))))
    (if (not (<= 1 mode 8))
	(error "mode ~S not 1-8" mode)
	(set! mode (- mode 1))) ; convert 1-8 to 0-7
    (if (logtest mode 4) ; is 4's bit on
	(set! modified (spectrum-amps spec))
	(set! modified (spectrum-freqs spec)))
    (if (logtest mode 2) ; is 2's bit on?
	(set! modifier (spectrum-amps spec))
	(set! modifier (spectrum-freqs spec)))
    (if (logtest mode 1)
	(set! scaling #t)
	(set! scaling #f))
    (cond ((number? args); value
	   (if scaling
	       (do ((tail modified (cdr tail)))
		   ((null? tail) #f)
		 (set-car! tail (* (car tail) args)))
	       (do ((tail modified (cdr tail)))
		   ((null? tail) #f)
		 (set-car! tail args))))
	  (else
	   (if (not (every? number? args))
	       ;; replace #f and #t with appropriate min max values
	       (let ((xmin #f) (xmax #f) (ymin #f) (ymax #f))
		 (if (logtest mode 1) ; env x is amp
		     (begin (set! xmin (spectrum-minamp spec))
			    (set! xmax (spectrum-maxamp spec)))
		     (begin (set! xmin (spectrum-minfreq spec))
			    (set! xmax (spectrum-maxfreq spec))))
		 (if (logtest mode 2) ; env y is amp
		     (begin (set! ymin (spectrum-minamp spec))
			    (set! ymax (spectrum-maxamp spec)))
		     (begin (set! ymin (spectrum-minfreq spec))
			    (set! ymax (spectrum-maxfreq spec))))
		 (set! args (append args (list))) ; side effect copy!
		 (do ((tail args (cddr tail)))
		     ((null? tail) #f)
		   (cond ((eqv? (car tail) #f) 
			  (set-car! tail xmin))
			 ((eqv? (car tail) #t) 
			  (set-car! tail xmax)))
		   (cond (scaling
			  (error "non-numerical y value ~S can't be scaler"
				 (cadr tail)))
			 ((eqv? (cadr tail) #f) 
			  (set-car! (cdr tail) ymin))
			 ((eqv? (cadr tail) #t) 
			  (set-car! (cdr tail) ymax))))))
	   ;; now have valid envelope
	   (if scaling
	       (do ((tail modified (cdr tail)))
		   ((null? tail) #f)
		 (set-car! tail (* (car tail) 
				   (interp (car tail) args))))
	       (do ((tail modified (cdr tail)))
		   ((null? tail) #f)
		 (set-car! tail (interp (car tail) args))))
	   (if (not (apply <= (spectrum-freqs spec)))
	       (spectrum-sort! spec))))
    spec))

; (define aaa (make-spectrum #f 6 '(100 200 300 400 500 800) '(0 0 0 0 0 0)))
; (spectrum-freqs aaa)
; (spectrum-rescale! aaa 1 100 800 800 100)
; (spectrum-freqs aaa)

(define (sdif-import file . sig)
  (if (and (pair? sig) (null? (cdr sig)))
      (set! sig (car sig)))
  (ffi_sdif_import file sig))

; (sdif-import "/Users/hkt/00log/log-drum-1.sdif") 
; (sdif-import-spectra "/Users/hkt/00log/log-drum-1.sdif") 

(define (sdif-import-spectra file)
  ;; iterate all 1TRC frames in file and convert to spectrum objects,
  ;; each frame in file is ("1TRC" time ("1TRC" (1 2 3 4) (1 2 3 4)))
  (do ((frames (sdif-import file "1TRC") (cdr frames))
       (head #f)
       (tail #f))
      ((null? frames)
       (or head (error "sdif-import-spectra: no 1TRC data in ~S" file)))
    (let ((time (second (car frames))) ; time is second frame element
          (rows (cdr (third (car frames)))) ; should only be 1 matrix
          (frqs #f)
          (amps #f)
          (spec #f) )
      ;; sort rows according to freq, which is second value in row
      (set! rows (qsort! rows (lambda (a b) (< (second a) (second b)))))
      (set! frqs (map second rows))
      (set! amps (map third rows))
      (set! spec (make-spectrum time (length rows) frqs amps))
      (if (not head)
          (begin (set! head (list spec)) (set! tail head))
          (begin (set-cdr! tail (list spec)) (set! tail (cdr tail)))))))

(define (read-spear-frame str)
  (let ((port (open-input-string str))
	(rdat (lambda (p)
		(let ((x (read p)))
		  (if (eof-object? x)
		      (error "~S is not frame data" str))
		  x)))
	(time #f)
	(size #f)
	(amps (list #f))
	(frqs (list #f)))
    ;; read time and num partials
    (set! time (rdat port))
    (set! size (rdat port))
    (do ((i 0 (+ i 1))
	 (a amps)
	 (f frqs))
	((= i size) #f)
      (rdat port) ; flush partial num
      (set-cdr! f (list (rdat port))) ; read freq
      (set! f (cdr f))
      (set-cdr! a (list (rdat port))) ; read amp
      (set! a (cdr a)))
    (if (null? (cdr frqs)) ; omit null frames
	#f ;;(list (cdr frqs) (cdr amps))
	(make-spectrum time size (cdr frqs) (cdr amps)))))

; (read-spear-frame "0.000000 13 12 170.647339 0.045844 11 209.358994 0.036739 10 318.045227 0.246056 9 363.138550 0.098190 8 449.606598 0.021067 7 534.593201 0.010766 6 668.234375 0.006407 5 1133.600830 0.019034 4 1230.239136 0.003197 3 1471.668579 0.001610 2 1626.804565 0.002571 1 2431.637695 0.001024 0 3032.626221 0.001559")

(define (import-spear-frames file)
  (format #t "import-spear-frames has been renamed spear-import-spectra" )
  (spear-import-spectra file)
  )

(define (spear-import-spectra file)
  (with-input-from-file file
    (lambda ()
      (let ((port (current-input-port)))
	;; read/check frame file header
	(let ((rhdr (lambda (p)
		      (let ((l (read-line p)))
			(if (eof-object? l)
			    (error "Reached EOF in file header." p))
			l ;(linenoeol l)
			)))
	      (line #f))
	  (set! line (rhdr port))
	  (if (not (equal? line "par-text-frame-format"))
	      (error "Expected 'par-text-frame-format' but got '~A'" line))
	  (set! line (rhdr port))
	  (if (not (equal? line 
			   "point-type index frequency amplitude"))
	      (error "Expected 'point-type index frequency amplitude' but got '~A'." line))
	  ;; flush remaining header lines
	  (do ()
	      ((equal? line "frame-data") #f)
	    (set! line (rhdr port))))
	;; file now at frame-data, read spectra till eof
	(let* ((head (list #f))
	       (tail head))
	  (do ((line (read-line port) (read-line port))
	       (spec #f))
	      ((eof-object? line)
	       (cdr head))
	    ;; omit empty spectra
	    (set! spec (read-spear-frame line)) ;(linenoeol line)
	    (if spec
		(begin
		  (set-cdr! tail (list spec))
		  (set! tail (cdr tail))))))))))

(define (export-spear-frames frames file)
  ;; dump specta in Spear import file format. since spectra do not
  ;; keep track of partial numbers this is a bit of a kludge (the
  ;; partial number is simply the position of the frq amp pair in the
  ;; spectral lists)

  (if (not (pair? frames))
      (set! frames (list frames)))
  (let ((partials 0)
	(numframes (length frames)))
    ;; find max num partials
    (do ((tail frames (cdr tail)))
	((null? tail) #f)
      (set! partials (max partials (length (spectrum-freqs (car tail))))))
    (with-output-to-file file
      (lambda ()
	(let ((port (current-output-port)))
	  (format port "par-text-frame-format~%")
	  (format port "point-type index frequency amplitude~%")
	  (format port "partials-count ~S~%" partials)
	  (format port "frame-count ~S~%" numframes)
	  (format port "frame-data~%")
	  (do ((tail frames (cdr tail))
	       (count 0 (+ count 1)))
	      ((null? tail)
	       #f)
	    (format port "~S ~S" (or (spectrum-time (car tail))
				     (* count 1.0))
		    (spectrum-size (car tail)))
	    (do ((freqs (spectrum-freqs (car tail)) (cdr freqs))
		 (amps (spectrum-amps (car tail)) (cdr amps))
		 (size (spectrum-size (car tail)) (- size 1)))
		((<= size 0) #f)
	      (format port " ~S ~S ~S" (- size 1) (car freqs) (car amps)))
	    (format port "~%")))))))

(define (clm-frame->spectrum data . args)
  (with-optkeys (args (fundamental 1) (amplitude 1))
     (let* ((frqs (list #f))
            (tail1 frqs)
            (amps (list #f))
            (tail2 amps))
       (do ((dat data (cddr dat))
            (siz 0 (+ siz 1)))
           ((null? dat)
            (make-spectrum 0 siz (cdr frqs) (cdr amps)))
         (set-cdr! tail1 (list (* (car dat) fundamental)))
         (set! tail1 (cdr tail1))
         (set-cdr! tail2 (list (* (cadr dat) amplitude)))
         (set! tail2 (cdr tail2))))))

;;;  July 29, 1998
;;;
;;;  Main function is 'acoustic-dissonance.'  See documentation for the
;;;  function for examples of use.  Don't forget that in MCL the command
;;;  "control - x -d" will open a documentation window. This program is 
;;;  based on the work of Hutchinson & Knopoff (1978) as slightly modified 
;;;  by Richard Parncutt. You are welcome to use it in any way you like
;;;  but I take no responsibility for any lame implementation choices
;;;  I may have made...
;;;
;;;					-Sean Ferguson
;;;					 McGill University
;;;					 Montreal, Canada
;;;					 ferguson@music.mcgill.ca

#|
The references are:

Hutchinson, William and Leon Knopoff. "The Acoustic Component of Western
Consonance." Interface, Vol. 7 (1978), pp. 1-29.

Hutchinson, William and Leon Knopoff. "The significance of the acoustic
component of dissonance in Western triads." Journal of Musicological
Research, Vol. 3, 1979, pp. 5-22

Thompson, William Forde and Richard Parncutt. "Perceptual Judgments of
Triads and Dyads: Assessment of a Psychoacoustic Model." Music Perception,
Spring 1997, Vol. 14, No. 3, 263-280.

Parncutt, Richard and Hans Strasburger. "Applying Psychoacoustics in
Composition: 'Harmonic' Progression of 'Nonharmonic' Sonorities." This is
in a relatively recent volume of Perspectives of New Music, but
unfortunately I don't have the exact year with me. It is after 1990, in
any case.

Parncutt, Richard. Harmony: a Psychoacoustical Approach. Springer-Verlag
1989

Danner, Gregory. "The Use of Acoustic Measures of Dissonance to
Characterize Pitch-Class Sets." Music Perception Vol. 3, No. 1, 103-122,
Fall 1985.

Balsach, Llorenc. "Application of Virtual Pitch Theory in Music Analysis.
Journal of New Music Research, Vol. 26 (1997), pp. 244-265.

It is probably best to start with the Hutchinson and Knopoff and then just
muck around after that. The Parncutt book is also very good.
|#

(define (midi->pitcat midi-pitch)
  ;; Converts MIDI pitch numbers (middle c = 60) to Parncutt's pitch
  ;; categories (middle c = 48)
  (- midi-pitch 12))

; (midi->pitcat 60)

;;Taken from Parncutt...

;(define (pitch->Hz midi-pitch)
; ;; Translates a MIDI pitch number (middle c = 60) into a frequency
;  ;; given in Hz. So (pitch->Hz 69) returns 440 Hz."
;  (* 440 (expt 2.0 (/ (- (midi->pitcat midi-pitch) 57) 12))))

; (pitch->Hz 69)
; (pitch->Hz 60)

(define (mean-freq f1 f2)
  ;; Returns the mean frequency of two pure tones in Hz.  Equals 1/2(f1 + f2).
  (/ (+ f1 f2) 2.0))

; (mean-freq 440 100)

(define (critical-bandwidth f)
  ;; Returns the critical bandwidth in Hz for the given frequency,
  ;; also in Hz (a4 = 69 = 440 Hz)."
  (* 1.72 (expt f .65)))

; (critical-bandwidth 440)
; (critical-bandwidth (pitch->Hz 30))

(define (cbw-interval f1 f2)
  ;; Gives the interval between two partials in units 
  ;; of the critical bandwidth. Frequencies should be given
  ;; in Hz.
  (/ (abs (- f2 f1))
     (critical-bandwidth (mean-freq f1 f2)))) 

; (cbw-interval 440 100)
; (cbw-interval 440 (pitch->Hz 60))

(define (standard-curve cbw-int)
  ;; Parncutt's function for g(y) of H&K (p. 4). Gives the dissonance
  ;; weighting factor of the frequency difference of two pure tones in 
  ;; units of the critical bandwidth."
  (if (> cbw-int 1.2) ;if critical bandwidth interval > 1.2, *no* roughness
    0
    (let ((ratio (/ cbw-int .25))) ;.25 is interval for max roughness
      (expt (* (* 2.7182818 ratio)
               (exp (* -1 ratio)))
            2))))

; (standard-curve (cbw-interval 440 400))
; (standard-curve (cbw-interval 440 500))

(define (pure-tone-dissonance f1 f2)
  ;; Gives the pure tone dissonance of two partials given in Hz without 
  ;; considering amplitude. That is, amp for both f1 and f2 = 1.
  (standard-curve (cbw-interval f1 f2)))

; (pure-tone-dissonance 440 400)
; (pure-tone-dissonance 440 500)

(define (harmonic-interval harmonic-number)
  ;; Gives the interval in semitones between fundamental and harmonic.
  (floor (+ (/ (* 12 (log harmonic-number)) (log 2)) .5)))

; (harmonic-interval 23)

(define (harmonic-series-pitch fundamental-pitch . no-of-harms)
  ;; Give harmonic series rounded off to chromatic pitches.
  (set! no-of-harms (if (null? no-of-harms) 10 (car no-of-harms)))
  (let* ((head (list #f))
         (tail head))
    (do ((x 1 (+ x 1)))
        ((= x no-of-harms)
         (cdr head))
      (set-cdr! tail (list (+ (harmonic-interval x) fundamental-pitch)))
      (set! tail (cdr tail)))))

; (harmonic-series-pitch 36 11)

(define (harmonic-series-frequency fundamental-pitch . no-of-harms )
  ;; Gives the harmonic series in frequency rounded off to chromatic pitches.
  ;; Fundamental pitch is given as MIDI note number (middle c = 60).
  (set! no-of-harms (if (null? no-of-harms) 10 (car no-of-harms)))
  (map key (harmonic-series-pitch fundamental-pitch no-of-harms)))

; (harmonic-series-frequency 36 11)

(define (sum-amplitudes amp1 amp2)
  ;; Returns the summed amplitudes of arguments.  Amplitudes are 
  ;; given as fractions of harmonic numbers (1/n). For example, 
  ;; the amplitude of harmonic 3 is 1/3.
  (sqrt (+ (* amp1 amp1) (* amp2 amp2))))

; (sum-amplitudes .2 .1)

(define (get-harm-amps harm-series)
  (let* ((head (list #f))
         (tail head))
    (do ((walk harm-series (cdr walk))
         (x 1 (+ x 1)))
        ((null? walk)
         (cdr head))
      (set-cdr! tail (list (cons (car walk) (/ 1 x))))
      (set! tail (cdr tail)))))

; (get-harm-amps '(1 2 3 4 5 6 7))

(define (get-amps-of-overlaps overlaps spect1+amps spect2+amps)
  ;; To be called by merge-spectrums only.
  (do ((walk overlaps (cdr walk))
       (result (list )))
      ((null? walk) result)
    (let ((x (car walk)))
      (set! result (cons (cons x (sum-amplitudes
                                  (cdr (assoc x spect1+amps))
                                  (cdr (assoc x spect2+amps))))
                         result)))))

(define (remove-overlaps orig final pool)
  (cond ((null? orig) final)
        ((member (caar orig) pool)
         (remove-overlaps (cdr orig) final pool))
        (else
         (remove-overlaps (cdr orig)
                          (cons (car orig) final)
                          pool))))
              
; (list-intersection '() '())
; (list-intersection '(1.0) '())
; (list-intersection '(0.0 1.0 2.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0 4.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0 4.0) '(0.1 1.0 2.1))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.1 a b) (1.0 a b) (2.1 a b)) :key car)
; (list-intersection '((1.0 a b)) '(()))
; (list-intersection '((0.0 a b) (1.0 a b) (2.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.1 a b) (1.0 a b) (2.1 a b)) :key car)

; (list-intersection '( (60 . 1) (72 . 1/2) (79 . 1/3) (84 . 1/4) (88 . 1/5) (91 . 1/6) (94 . 1/7) (96 . 1/8) (98 . 1/9) (100 . 1/10)) '( (61 . 1) (73 . 1/2) (80 . 1/3) (85 . 1/4) (89 . 1/5) (92 . 1/6) (95 . 1/7) (97 . 1/8) (99 . 1/9) (101 . 1/10)) )

;;This isn't that efficient, but it isn't really called very often.
(define (merge-spectrums fund-pitch1 . pitches)
  ;; Returns a list of dotted pairs with car = pitch of spectral
  ;; component (in MIDI numbers) and cdr = relative amplitude.
  ;; Components which occur more than once have their amplitudes
  ;; scaled appropriately.
  (if (not (null? pitches)) (set! pitches (car pitches)))
  (let ((result (get-harm-amps (harmonic-series-pitch fund-pitch1))))
    (if (null? pitches) 
        result
        (letrec ((rec1 (lambda (final current pchs)
                         (let* ((spect+amps (get-harm-amps (harmonic-series-pitch current)))
                                (overlaps (map car 
                                               (list-intersection final spect+amps :getter car)))
                                (overlap+amps (get-amps-of-overlaps overlaps final spect+amps))
                                (merged-spects+amps (append final spect+amps))
                                (gapped-spects+amps (remove-overlaps merged-spects+amps
                                                                     (list)
                                                                     overlaps))
                                (re-merged (sort (append overlap+amps
                                                         gapped-spects+amps)
                                                 (lambda (a b) (< (car a) (car b)))
                                                 )))
                           (cond ((null? pchs) re-merged)
                                 (else (rec1 re-merged
                                             (car pchs)
                                             (cdr pchs))))))))
          (rec1 result (car pitches) (cdr pitches))
          
          ))))

(merge-spectrums 60 '(61 62 73))
; (merge-spectrums 62 '(67 72 77 82))

(define (not-too-big-p x y)
  ;; Tests to see if two MIDI pitches are small enough to
  ;; bother testing for pure-tone-dissonance.  A time-
  ;; saving function for 'acoustic-dissonance.'"
  (let ((diff (abs (- x y)))
        (lower (min x y)))
    (or (and (< lower 24) (< diff 11)) 
        (and (< lower 36) (< diff 10))
        (and (< lower 48) (< diff 8))
        (and (< lower 60) (< diff 7))
        (and (< lower 72) (< diff 5))
        (and (< lower 86) (< diff 4))
        (and (< lower 106) (< diff 3))
        (and (> lower 105) (< diff 2)))))
                              
(define (diss-numerator pch-amp1 pch-amp2)
  ;; Pitches must be given as MIDI notes.
  (let ((pch1 (car pch-amp1))
        (pch2 (car pch-amp2))
        (amp1 (cdr pch-amp1))
        (amp2 (cdr pch-amp2))
        )
    ;;This conditional statement increases speed by up to 5 times or more!
    (if (not (not-too-big-p pch1 pch2)) ;Check to see if worth computing,
        0 				;if not, then return 0.
        (* (* amp1 amp2) ;Numerator 	;Otherwise, calculate numerator
           (pure-tone-dissonance (keynum pch1)
                                 (keynum pch2)))))) ;Numerator

(define (acoustic-dissonance pitches) 
  ;; Given a list of one or more midi pitches, returns the
  ;; acoustic-dissonance, or 'roughness.' For example:
  ;; ? (acoustic-dissonance '(60 61 62 73))
  ;; 0.7287
  ;; ? (acoustic-dissonance '(60))
  ;; 0.0012
  ;; ? (acoustic-dissonance '(47 49 53))
  ;; 0.3846
  ;; ? (acoustic-dissonance '(62 67 72 77 82))
  ;; 0.1592
  (let* ((combined-spectrum (merge-spectrums (first pitches) (rest pitches)))
         ;;denominator is total amplitude of spectrum
         (denominator (apply + (map (lambda (x) (* x x)) 
                                    (map cdr combined-spectrum))))
         ;; do* variables
         (comb-spect combined-spectrum )
         (lowest (car combined-spectrum) )
         (higher (cdr combined-spectrum) )
         (interim-list (map (lambda (x) (diss-numerator lowest x)) higher) )
         (result interim-list ) )
    (do ()
        ((null? (rest comb-spect))
         (decimals (/ (apply + result) denominator)  4))    ;add up all numerators and then divide
      (set! comb-spect (cdr comb-spect))
      (set! lowest (car comb-spect))
      (set! higher (cdr comb-spect))
      (set! interim-list (map (lambda (x) (diss-numerator lowest x)) higher))
      (set! result (append interim-list result))))
  )

(define (acoustic-sort chords . cons-to-diss)
  (set! cons-to-diss
        (if (and (pair? cons-to-diss) (eq? (car cons-to-diss) #f))
            > <))
  (let ((rank (map (lambda (x) (cons (acoustic-dissonance x) x)) chords)))
    (map cdr (sort rank (lambda (a b) (cons-to-diss (car a) (car b)))))))







