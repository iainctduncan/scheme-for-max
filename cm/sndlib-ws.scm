;;; with-sound for a sndlib-only context (no Snd editor)

(provide 'sndlib-ws.scm)

(set! *clm-srate* 44100)

(define *clm-file-name* "test.snd")
(define *clm-channels* 1)
(define *clm-data-format* mus-lfloat)
(define *clm-header-type* mus-next)
(define *clm-verbose* #f)
(define *clm-play* #f)
(define *clm-statistics* #f)
(define *clm-reverb* #f)
(define *clm-reverb-channels* 1)
(define *clm-reverb-data* ())
(define *clm-locsig-type* mus-interp-linear)
(define *clm-clipped* #t)
(define *clm-array-print-length* 12)
(define *clm-player* #f) 
(define *clm-notehook* #f)
(define *clm-with-sound-depth* 0) ; for CM, not otherwise used
(define *clm-delete-reverb* #f)   ; should with-sound clean up reverb stream

(set! *clm-file-buffer-size* 65536)

(define (times->samples beg dur) 
  "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"
  (list (seconds->samples beg) (seconds->samples (+ beg dur))))


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05

(define *definstrument-hook* #f) ; for CM

(define-macro (definstrument args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names ()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a)) ; redundant :optional or something
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names))))
  `(begin 
     (define* (,name ,@targs)
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       ,@body)
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))



;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    (srate *clm-srate*) 
			    (output *clm-file-name*) 
			    (channels *clm-channels*)
			    (header-type *clm-header-type*)
			    (data-format *clm-data-format*)
			    (comment #f)
			    (verbose *clm-verbose*)
			    (reverb *clm-reverb*)
			    (revfile "test.rev")
			    (reverb-data *clm-reverb-data*)
			    (reverb-channels *clm-reverb-channels*)
			    (continue-old-file #f)
			    (statistics *clm-statistics*)
			    (scaled-to #f)
			    (scaled-by #f)
			    (play *clm-play*)
			    (clipped 'unset)
			    (notehook *clm-notehook*)               ; (with-sound (:notehook (lambda args (display args))) (fm-violin 0 1 440 .1))
			    (ignore-output #f))
  "with-sound-helper is the business portion of the with-sound macro"
  (let* ((old-srate *clm-srate*)
	 (old-*output* *output*)
	 (old-*reverb* *reverb*)
	 (old-notehook *clm-notehook*)
	 (old-verbose *clm-verbose*)
	 (output-to-file (string? output))
	 (output-1 (if (and output-to-file
			    (or scaled-to scaled-by))
		       (string-append output ".temp") 
		       output))                    ; protect during nesting
	 (reverb-1 revfile)
	 (reverb-to-file (and reverb (string? revfile))))

    (if ignore-output
	(begin
	  (set! output-1 *clm-file-name*)
	  (set! output-to-file (string? output-1))))

    (dynamic-wind 

     (lambda () 
       (set! *clm-verbose* verbose)
       (set! *clm-notehook* notehook)
       (set! (locsig-type) *clm-locsig-type*)
       (set! (mus-array-print-length) *clm-array-print-length*)
       (if (equal? clipped 'unset)
	   (if (and (or scaled-by scaled-to)
		    (member data-format (list mus-bfloat mus-lfloat mus-bdouble mus-ldouble)))
	       (set! (mus-clipping) #f)
	       (set! (mus-clipping) *clm-clipped*))
	   (set! (mus-clipping) clipped))
       (set! *clm-srate* srate))

     (lambda ()
       (if output-to-file
	   (begin
	     (if continue-old-file
		 (begin
		   (set! *output* (continue-sample->file output-1))
		   (set! *clm-srate* (mus-sound-srate output-1)))
		 (begin
		   (if (file-exists? output-1) 
		       (delete-file output-1))
		   (set! *output* (make-sample->file output-1 channels data-format header-type comment)))))
	   (begin
	     (if (and (not continue-old-file)
		      (vector? output-1))
		 (fill! output-1 0.0))
	     (set! *output* output-1)))
       
       (if reverb
	   (if reverb-to-file
	       (begin
		 (if continue-old-file
		     (set! *reverb* (continue-sample->file reverb-1))
		     (begin
		       (if (file-exists? reverb-1) 
			   (delete-file reverb-1))
		       (set! *reverb* (make-sample->file reverb-1 reverb-channels data-format header-type)))))
	       (begin
		 (if (and (not continue-old-file)
			  (vector? reverb-1))
		     (fill! reverb-1 0.0))
		 (set! *reverb* reverb-1))))

       (let ((start (if statistics (get-internal-real-time)))
	     (flush-reverb #f)
	     (cycles 0)
	     (revmax #f))

	 (catch 'mus-error
		thunk
		(lambda args
		  (format #t ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args))
		  (set! flush-reverb #t)))
		  
	 (if (and reverb 
		  (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	     (begin
	       (if reverb-to-file
		   (mus-close *reverb*))
	       (if statistics 
		   (if reverb-to-file
		       (set! revmax (cadr (mus-sound-maxamp reverb-1)))
		       (if (float-vector? reverb-1)
			   (set! revmax (float-vector-peak reverb-1)))))
	       (if reverb-to-file
		   (set! *reverb* (make-file->sample reverb-1)))
	       (apply reverb reverb-data)                                   ; here is the reverb call(!)
	       (if reverb-to-file
		   (mus-close *reverb*))
	       ))

	 (if output-to-file
	     (mus-close *output*))

	 (if statistics 
	     (begin
	       (set! cycles (exact->inexact (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
	       (format #t "~%;~A:~%  maxamp~A:~{ ~,4F~}~%~A  compute time: ~,3F~%"
		       (if output-to-file
			   (if (or scaled-to scaled-by)
			       (substring output-1 0 (- (string-length output-1) 5))
			       output-1)
			   (if (vector? output-1) "vector" "flush"))
		       (if (or scaled-to scaled-by) 
			   " (before scaling)" 
			   "")
		       (if output-to-file
			   (let ((lst (mus-sound-maxamp output-1)))
			     (do ((i 0 (+ i 2)))
				 ((>= i (length lst)))
			       (list-set! lst i (/ (list-ref lst i) *clm-srate*)))
			     lst)
			   (if (float-vector? output-1)
			       (list (float-vector-peak output-1))
			       '(0.0)))
		       (if revmax (format #f "  rev max: ~,4F~%" revmax) "")
		       cycles)))

	 (if (or scaled-to scaled-by)
	     (if output-to-file
		 (let ((scaling
			(or scaled-by
			    (let* ((mx-lst (mus-sound-maxamp output-1))
				   (mx (if (not (null? mx-lst)) (cadr mx-lst) 1.0)))
			      (do ((i 1 (+ i 2)))
				  ((>= i (length mx-lst)) (/ scaled-to mx))
				(set! mx (max mx (list-ref mx-lst i)))))))
		       (out-file (substring output-1 0 (- (string-length output-1) 5))))
		   (mus-sound-close-output (mus-sound-open-output out-file srate channels data-format header-type) 0)
		   (mus-mix out-file output-1 0 (mus-sound-frames output-1) 0 (make-scalar-mixer channels scaling))
		   (delete-file output-1)
		   (set! output-1 (substring output-1 0 (- (string-length output-1) 5))))

		 (if (float-vector? output-1)
		     (if scaled-to
			 (let ((pk (float-vector-peak output-1)))
			   (if (> pk 0.0)
			       (float-vector-scale! output-1 (/ scaled-to pk))))
			 (float-vector-scale! output-1 scaled-by)))))

	 (if (and *clm-player* play output-to-file)
	     (*clm-player* output-1)))

       output-1)

     (lambda () 
       (set! *clm-verbose* old-verbose)
       (set! *clm-notehook* old-notehook)
       (if *reverb*
	   (begin
	     (mus-close *reverb*)
	     (set! *reverb* old-*reverb*)))
       (if *output*
	   (begin
	     (if (mus-output? *output*)
		 (mus-close *output*))
	     (set! *output* old-*output*)))
       (set! *clm-srate* old-srate)))))


(define-macro (with-sound args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))



;;; -------- with-temp-sound --------

(define-macro (with-temp-sound args . body)
  `(let ((old-file-name *clm-file-name*))
     ;; with-sound but using tempnam for output (can be over-ridden by explicit :output)
     (dynamic-wind
	 (lambda () 
	   (set! *clm-file-name* (tmpnam)))
	 (lambda ()
	   (with-sound-helper (lambda () ,@body) ,@args)) ; dynamic-wind returns this as its result
	 (lambda ()
	   (set! *clm-file-name* old-file-name)))))


;;; -------- clm-load --------

(define (clm-load file . args) 
  "(clm-load file . args) loads 'file' in the context of with-sound"
  (apply with-sound-helper (lambda () (load file)) args))



;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(define-macro (sound-let snds . body) 
  `(let ((temp-files ()))
     (begin
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (and (string? file)          ; is it a file? 
			      (file-exists? file))
			 (delete-file file)))
		   temp-files)
	 val))))                                      ; return body result



;;; -------- Common Music --------

(define* (init-with-sound
	  (srate *clm-srate*) 
	  (output *clm-file-name*) 
	  (channels *clm-channels*)
	  (header-type *clm-header-type*)
	  (data-format *clm-data-format*)
	  (comment #f)
	  ;(verbose *clm-verbose*) ; why is this commented out?
	  (reverb *clm-reverb*)
	  (revfile "test.rev")
	  (reverb-data *clm-reverb-data*)
	  (reverb-channels *clm-reverb-channels*)
	  (continue-old-file #f)
	  (statistics *clm-statistics*)
	  (scaled-to #f)
	  (play *clm-play*)
	  (scaled-by #f))
  "(init-with-sound . args) is the first half of with-sound; it sets up the CLM output choices, reverb, etc. Use \
finish-with-sound to complete the process."
  (let ((old-srate *clm-srate*)
	(start (if statistics (get-internal-real-time)))
	(output-to-file (string? output))
	(reverb-to-file (and reverb (string? revfile))))
    (set! *clm-srate* srate)
    (if output-to-file
	(if continue-old-file
	    (begin
	      (set! *output* (continue-sample->file output))
	      (set! *clm-srate* (mus-sound-srate output)))
	    (begin
	      (if (file-exists? output) 
		  (delete-file output))
	      (set! *output* (make-sample->file output channels data-format header-type comment))))
	(begin
	  (if (and (not continue-old-file)
		   (vector output))
	      (fill! output 0.0))
	  (set! *output* output)))

    (if reverb
	(if reverb-to-file
	    (if continue-old-file
		(set! *reverb* (continue-sample->file revfile))
		(begin
		  (if (file-exists? revfile) 
		      (delete-file revfile))
		  (set! *reverb* (make-sample->file revfile reverb-channels data-format header-type))))
	    (begin
	      (if (and (not continue-old-file)
		       (vector? revfile))
		  (fill! revfile 0.0))
	      (set! *reverb* revfile))))

    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  #f ;to-snd
	  scaled-to
	  scaled-by
	  play
	  reverb-data
	  start)))

(define (finish-with-sound wsd)
  "(finish-with-sound wsd) closes the notelist process started by init-with-sound"
  (if (eq? (car wsd) 'with-sound-data)
      (let ((output (list-ref wsd 1))
	    (reverb (list-ref wsd 2))
	    (revfile (list-ref wsd 3))
	    (old-srate (list-ref wsd 4))
	    ;(statistics (list-ref wsd 5))
	    ;(to-snd (list-ref wsd 6))
	    ;(scaled-to (list-ref wsd 7))
	    ;(scaled-by (list-ref wsd 8))
	    ;(play (list-ref wsd 9))
	    (reverb-data (list-ref wsd 10))
	    ;(start (list-ref wsd 11))
	    )

	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (if (string? revfile)
		  (set! *reverb* (make-file->sample revfile))
		  (set! *reverb* revfile))
	      (apply reverb reverb-data)
	      (mus-close *reverb*)))
	(if (mus-output? *output*)
	    (mus-close *output*))

	(set! *clm-srate* old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))


(define wsdat-play ; for cm
  (make-procedure-with-setter
   (lambda (w)
     "accessor for play field of init-with-sound struct"
     (list-ref w 9))
   (lambda (w val)
     (list-set! w 9 val))))


(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 (/ 57 12)))) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                             ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0)))

    (define (string-downcase str) 
      (apply string (map char-downcase str)))

    (lambda* (pitch pythagorean)          ; pitch can be pitch name or actual frequency
      "(->frequency pitch pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."
      (if (symbol? pitch)
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (base-char (name 0))
		 (sign-char (and (> (string-length name) 1)
				 (not (char-numeric? (name 1)))
				 (not (char=? (name 1) #\n))
				 (name 1)))
		 (octave-char (if (and (> (string-length name) 1)
				       (char-numeric? (name 1))) 
				  (name 1)
				  (if (and (> (string-length name) 2) 
					   (char-numeric? (name 2)))
				      (name 2)
				      #f)))
		 (base (modulo (+ 5 (- (char->integer base-char) (char->integer #\a))) 7)) ; c-based (diatonic) octaves
		 (sign (if (not sign-char) 0 (if (char=? sign-char #\f) -1 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (+ sign (case base ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (if pythagorean
		(* main-pitch (expt 2 octave) (ratios base-pitch))
		(* main-pitch (expt 2.0 (/ et-pitch 12)))))
	  pitch))))


(define (->sample beg)
  "(->sample time-in-seconds) -> time-in-samples"
  (round (* (if (not (null? (sounds))) (srate) *clm-srate*) beg)))


;;; -------- defgenerator --------

;;; (defgenerator osc a b)
;;; (defgenerator (osc :methods (list (cons 'mus-frequency (lambda (obj) 100.0)))) a b)

(define-macro (defgenerator struct-name . fields)

  (define (list->bindings lst)
    (if (null? lst)
	()
	(cons (if (pair? (car lst))
		  (list 'cons (list 'quote (caar lst)) (caar lst))
		  (list 'cons (list 'quote (car lst)) (car lst)))
	      (list->bindings (cdr lst)))))

  (let* ((name (if (list? struct-name) (car struct-name) struct-name))
	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (struct-name 1) :make-wrapper)
				    (struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (struct-name 3) :make-wrapper)
				    (struct-name 4))))
		      (lambda (gen) gen)))

	 (sname (if (string? name) name (symbol->string name)))
	 (methods (and (list? struct-name)
		       (or (and (> (length struct-name) 2)
				(equal? (struct-name 1) :methods)
				(struct-name 2))
			   (and (= (length struct-name) 5)
				(equal? (struct-name 3) :methods)
				(struct-name 4))))))
    `(begin 
       (define ,(string->symbol (string-append sname "?")) #f)
       (define ,(string->symbol (string-append "make-" sname)) #f)

       (let ((gen-type (gensym)))
	 
	 (set! ,(string->symbol (string-append sname "?"))
	       (lambda (obj)
		 (and (environment? obj)
		      (eq? (obj 'mus-generator-type) gen-type))))

	 (set! ,(string->symbol (string-append "make-" sname))
	       (lambda* ,(map (lambda (n) 
				(if (list? n) n (list n 0.0)))
			      fields)
  	         (,wrapper 
		  (open-environment
		   ,(if methods
		       `(augment-environment 
			   (apply environment ,methods)
			 (environment ,@(list->bindings fields) (cons 'mus-generator-type gen-type)))
		       `(environment ,@(list->bindings fields) (cons 'mus-generator-type gen-type)))))))))))


;;; --------------------------------------------------------------------------------
;;;
;;; functions from Snd that are used in some instruments
;;;   these replacements assume that the Snd functions are not present

(define (file-name name) 
  (if (string? name) 
      (mus-expand-filename name) 
      (mus-file-name name)))

(define srate mus-sound-srate)

(define (channels . args)
  (let ((obj (car args)))
    (if (string? obj)
	(mus-sound-chans obj)
	(mus-channels obj))))

;;; I think length is handled by s7 for all types

(define (frames . args)
  (let ((obj (car args)))
    (if (string? obj)
	(mus-sound-frames obj)
	(length obj))))


(define snd-print display)
(define snd-warning display)
(define snd-display (lambda args (apply format (append (list #t) (cdr args)))))
(define (snd-error str) (error 'mus-error str))
(define snd-tempnam tmpnam)

