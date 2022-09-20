;;; r7rs compatibility

(require libc.scm stuff.scm)
(provide 'r7rs.scm)


(define (vector-map p . args) (apply vector (apply map p args)))
(define (string-map p . args) (apply string (apply map p args)))
(define vector-for-each for-each) 
(define string-for-each for-each) 


(define* (vector->string v (start 0) end) 
  (let ((stop (or end (length v)))) 
    (copy v (make-string (- stop start)) start stop)))

(define* (string->vector s (start 0) end)
  (let ((stop (or end (length s)))) 
    (copy s (make-vector (- stop start)) start stop)))

(define list-copy copy)
(define vector-copy string->vector)
(define r7rs-string-copy vector->string) ; the latter doesn't know its not a vector
(define r7rs-vector-fill! fill!) ; or do these return the sequence, not the filler?
(define r7rs-string-fill! fill!)

(define* (vector-copy! dest at src (start 0) end) ; apparently end is exclusive here?
  (let ((len (or end (length src))))
    (if (or (not (eq? dest src))
	    (<= at start))
	(do ((i at (+ i 1))
	     (k start (+ k 1)))
	    ((= k len) dest)
	  (set! (dest i) (src k)))
	(do ((i (- (+ at len) start 1) (- i 1))
	     (k (- len 1) (- k 1)))
	    ((< k start) dest)
	  (set! (dest i) (src k))))))

(define (r7rs-make-hash-table . args)
  (if (null? args)
      (#_make-hash-table)
      (if (procedure? (car args))
	  (#_make-hash-table (if (null? (cdr args)) 511 (cadr args)) (car args))
	  (apply #_make-hash-table args))))

(define bytevector byte-vector)
(define bytevector? byte-vector?)
(define make-bytevector make-byte-vector)
(define bytevector-ref byte-vector-ref)
(define bytevector-set! byte-vector-set!)
(define bytevector-copy! vector-copy!)
(define string-copy! vector-copy!)
(define (bytevector->list bv) (map values bv))


(define (boolean=? . args)
  (or (null? args)
      (and (boolean? (car args))
	   (or (null? (cdr args))
	       (every? (lambda (obj) (eq? (car args) obj)) (cdr args))))))

(define (symbol=? . args) 
  (or (null? args)
      (and (symbol? (car args))
	   (or (null? (cdr args))
	       (every? (lambda (obj) (eq? (car args) obj)) (cdr args))))))


(define char-foldcase char-downcase) 
(define string-foldcase string-downcase)
;;; these and the string functions in s7 are not unicode-aware.  To get true unicode
;;;   handling of the bytes, use the glib functions in libxg or use cload (see xgdata.scm).
(define (digit-value c) (and (char-numeric? c) (- (char->integer c) (char->integer #\0))))


(define (finite? n) (and (number? n) (not (nan? n)) (not (infinite? n))))
(define exact-integer? integer?)	
(define (exact-integer-sqrt i) (let ((sq (floor (sqrt i)))) (values sq (- i (* sq sq)))))
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square x) (* x x))
(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define floor-remainder modulo)
(define (floor-quotient x y) (floor (/ x y)))


(define (input-port-open? p) (not (port-closed? p))) 
(define (output-port-open? p) (not (port-closed? p))) 
(define (port? p) (or (input-port? p) (output-port? p)))
(define binary-port? port?)
(define textual-port? port?)
(define (close-port p) ((if (input-port? p) close-input-port close-output-port) p))
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define (call-with-port port proc) ((if (input-port? port) call-with-input-file call-with-output-file) port proc))


(define bytevector-u8-ref byte-vector-ref)
(define bytevector-u8-set! byte-vector-set!)
(define bytevector-u8 (dilambda (lambda (b k) (b k)) (lambda (b k c) (set! (b k) c))))
(define bytevector-length length)
(define bytevector-copy vector-copy!)
(define bytevector-append append)
(define write-bytevector write)
(define* (read-bytevector! bv port (start 0) end)
  (let ((lim (or end (length bv)))
	(pt (or port (current-input-port))))
    (do ((i start (+ i 1))
	 (c (read-byte pt) (read-byte pt)))
	((or (>= i lim)
	     (eof-object? c))
	 bv)
      (set! (bv i) c))))
(define* (read-bytevector k port)
  (read-bytevector! (make-byte-vector k) port))
(define (get-output-bytevector port) (string->byte-vector (get-output-string port)))
(define open-input-bytevector open-input-string)
(define open-output-bytevector open-output-string)
(define read-u8 read-byte)
(define write-u8 write-byte) 
(define u8-ready? char-ready?) 
(define peek-u8 peek-char)
(define* (utf8->string v (start 0) end) 
  (if (string? v)
      v
      (substring (byte-vector->string v) start (or end (length v)))))
(define* (string->utf8 s (start 0) end) 
  (if (byte-vector? s)
      s
      (string->byte-vector (utf8->string s start end))))
(define write-simple write)

(define (eof-object) #<eof>)
(define-macro (features) '*features*) ; needs to be the local *features*


(define (with-exception-handler handler thunk) (catch #t thunk handler))
(define raise error)
(define raise-continuable error)

(define-macro (guard results . body)
  `(let ((,(car results) (catch #t (lambda () ,@body) (lambda args (car args)))))
     (cond ,@(cdr results))))

(define (read-error? obj) (eq? (car obj) 'read-error))
(define (file-error? obj) (eq? (car obj) 'io-error))
(define (error-message obj) (apply format #f (cadr obj)))
(define error-irritants cdadr)


(define interaction-environment curlet)
;; for null-environment see stuff.scm
(define-macro (include . files) 
  `(begin
     ,@(map (lambda (file)
	      `(load ,file (outlet (curlet))))
	    files)))

(set! *#readers* (cons (cons #\; (lambda (s) (read) (values))) *#readers*))
;; I prefer (define-expansion (comment . stuff) (reader-cond (#t (values))))
;;   or (format #f "~^ this is a comment ")


(define-macro (define-values vars expression)
  `(if (not (null? ',vars))
       (varlet (curlet) ((lambda ,vars (curlet)) ,expression))))

#|
(define-macro (define*-values vars expression) ; same but allows defaults for the vars
  `(if (not (null? ',vars))
       (varlet (curlet) ((lambda* ,vars (curlet)) ,expression))))

(define-macro (define-values vars . body) ; but the spec says "<expression>" here
  `(apply begin (map (lambda (var val) `(define ,var ,val)) ',vars (list (begin ,@body)))))
|#

(define-macro (let-values vars . body)
  (if (and (pair? vars)
	   (pair? (car vars))
	   (null? (cdar vars)))
      `((lambda ,(caar vars)
	  ,@body)
	,(cadar vars))
      `(with-let (apply sublet (curlet)
			(list ,@(map (lambda (v)
				       `((lambda ,(car v)
					   (values ,@(map (lambda (name)
							    (values (symbol->keyword name) name))
							  (let args->proper-list ((args (car v)))
							    (cond ((symbol? args)	(list args))
								  ((not (pair? args))	args)
								  ((pair? (car args))	(cons (caar args) (args->proper-list (cdr args))))
								  (else                 (cons (car args) (args->proper-list (cdr args)))))))))
					 ,(cadr v)))
				     vars)))
	 ,@body)))

(define-macro (let*-values vals . body)
  (let ((args ())
	(exprs ()))
    (for-each
     (lambda (arg+expr)
       (set! args (cons (car arg+expr) args))
       (set! exprs (cons (cadr arg+expr) exprs)))
     vals)
    (let ((form `((lambda ,(car args) ,@body) ,(car exprs))))
      (if (not (null? (cdr args)))
	  (for-each
	   (lambda (arg expr)
	     (set! form (list (list 'lambda arg form) expr)))
	   (cdr args)
	   (cdr exprs)))
      form)))


;; case-lambda       
(define-macro (case-lambda . choices)
  `(lambda args
     (case (length args)
       ,@(map (lambda (choice)
		(if (or (symbol? (car choice))
			(negative? (length (car choice))))
		    `(else (apply (lambda ,(car choice) ,@(cdr choice)) args))
		    `((,(length (car choice))) 
		      (apply (lambda ,(car choice) ,@(cdr choice)) args))))
	      choices))))


;; parameters
(define* (make-parameter init converter)
  (let* ((convert (or converter (lambda (x) x)))
	 (old-values ()) ; see below -- this is part of the funclet
	 (value (convert init)))
    (lambda () value)))

(define-macro (parameterize vars . body)
  `(dynamic-wind
       (lambda ()
	 ,@(map (lambda (var)
		  `(with-let (funclet ,(car var))
		     (set! old-values (cons value old-values))
		     (set! value (convert ,(cadr var)))))
		vars))
       (lambda () 
         ,@body)
       (lambda ()
	 ,@(map (lambda (var)
		  `(with-let (funclet ,(car var))
		     (set! value (car old-values))
		     (set! old-values (cdr old-values))))
		vars))))


;; libraries
(apply define (symbol (object->string '(scheme base))) (inlet) ()) ; ignore (scheme base)
(apply define (symbol (object->string '(scheme read))) (inlet) ()) ; and so on... what a pile of baloney
(apply define (symbol (object->string '(scheme write))) (inlet) ()) 
(apply define (symbol (object->string '(scheme time))) (inlet) ()) 
(apply define (symbol (object->string '(scheme file))) (inlet) ()) 
(apply define (symbol (object->string '(scheme cxr))) (inlet) ()) 
(apply define (symbol (object->string '(scheme inexact))) (inlet) ()) 
(apply define (symbol (object->string '(scheme char))) (inlet) ()) 
(apply define (symbol (object->string '(scheme complex))) (inlet) ()) 

(define-macro (define-library libname . body) ; |(lib name)| -> environment
  `(define ,(symbol (object->string libname))
     (with-let (sublet (unlet) 
			 (cons 'import (symbol->value 'import))
			 (cons '*export* ())
			 (cons 'export (symbol->value 
					(define-macro (,(gensym) . names) 
					  `(set! *export* (append ',names *export*))))))
       ,@body
       (apply inlet
	      (map (lambda (entry)
		     (if (or (member (car entry) '(*export* export import))
			     (and (pair? *export*)
				  (not (member (car entry) *export*))))
			 (values)
			 entry))
		   (curlet))))))

(define-macro (import . libs)
  `(varlet (curlet)
     ,@(map (lambda (lib)
	      (case (car lib)
		((only) 
		 `((lambda (e names)
		     (apply inlet
			    (map (lambda (name)
				   (cons name (e name)))
				 names)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))
  
		((except)
		 `((lambda (e names)
		     (apply inlet
			    (map (lambda (entry)
				   (if (member (car entry) names)
				       (values)
				       entry))
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))
  
		((prefix)
		 `((lambda (e prefx)
		     (apply inlet
			    (map (lambda (entry)
				   (cons (string->symbol 
					  (string-append (symbol->string prefx) 
							 (symbol->string (car entry)))) 
					 (cdr entry)))
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (caddr ',lib)))
  
		((rename)
		 `((lambda (e names)
		     (apply inlet
			    (map (lambda (entry)
				   (let ((info (assoc (car entry) names)))
				     (if info
					 (cons (cadr info) (cdr entry))
					 entry))) ; I assume the un-renamed ones are included
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))

		(else
		 `(let ((sym (symbol (object->string ',lib))))
		    (if (not (defined? sym))
			(format () "~A not loaded~%" sym)
			(symbol->value sym))))))
	    libs)))


;; delay and force: ugh
;;   this implementation is based on the r7rs spec
(define-macro (delay-force expr) 
  `(make-promise #f (lambda () ,expr)))
(define-macro (r7rs-delay expr) ; "delay" is taken damn it
  (list 'delay-force (list 'make-promise #t (list 'lambda () expr))))
(define (make-promise done? proc) 
  (list (cons done? proc)))
(define (force promise)
  (if (caar promise)
      ((cdar promise))
      (let ((promise* ((cdar promise))))
        (if (not (caar promise))
            (begin
              (set-car! (car promise) (caar promise*))
              (set-cdr! (car promise) (cdar promise*))))
        (force promise))))


;; floor/ and truncate/ can't work as intended: they assume that multiple values 
;;   are not spliced.  The "division library" is a trivial, pointless micro-optimization.
;; and why no euclidean-rationalize or exact-integer-expt?
;;   (imagine what will happen when r8rs stumbles on the zoo of continued fraction algorithms!)

(define (jiffies-per-second) 1000)
(define (current-jiffy) (round (* (jiffies-per-second) (*s7* 'cpu-time))))
(define (current-second) (floor (*s7* 'cpu-time)))

(define (get-environment-variable x)
  (let ((val ((*libc* 'getenv) x)))
    (and (string? val)
	 (> (length val) 0)
	 val)))
(define get-environment-variables (*libc* 'getenvs))
(define (r7rs-file-exists? arg) (= ((*libc* 'access) arg (*libc* 'F_OK)) 0))
(define r7rs-delete-file (*libc* 'unlink))

(define (os-type) (car ((*libc* 'uname))))
(define (cpu-architecture) (cadr ((*libc* 'uname))))
(define (machine-name) (caddr ((*libc* 'uname))))
(define (os-version) (string-append (list-ref ((*libc* 'uname)) 3) " " (list-ref ((*libc* 'uname)) 4)))
(define (implementation-name) (copy "s7"))
(define (implementation-version) (substring (s7-version) 3 7))

;; command-line is problematic: s7 has no access to the caller's "main" function, and
;;   outside Windows, there's no reasonable way to get these arguments.
;;   in Linux, you might win with:

(define (command-line)
  (let ((lst ()))
    (with-input-from-file "/proc/self/cmdline"
      (lambda ()
	(do ((c (read-char) (read-char))
	     (s ""))
	    ((eof-object? c)
	     (reverse lst))
	  (if (char=? c #\null)
	      (begin
		(set! lst (cons s lst))
		(set! s ""))
	      (set! s (string-append s (string c)))))))))


;; other minor differences: 
;;  in s7, single-quote can occur in a name


;; records
(define-macro (define-record-type type make ? . fields)
  (let ((new-type (if (pair? type) (car type) type))
	(inherited (if (pair? type) (cdr type) ()))
	(obj (gensym))
	(new-obj (gensym)))
    `(begin
       (define-class ,new-type ,inherited   ; from stuff.scm
         (map (lambda (f) (if (pair? f) (car f) f)) ',fields))
       
       (define (,? ,obj)    ; perhaps the define-class type predicate should use this 
         (define (search-inherited ,obj type)
	   (define (search-inheritors objs type)
	     (and (pair? objs)
		  (or (search-inherited (car objs) type)
		      (search-inheritors (cdr objs) type))))
	   (or (eq? (,obj 'class-name) type)
	       (search-inheritors (,obj 'inherited) type)))
         (and (let? ,obj)
	      (search-inherited ,obj ',new-type)))
       
       (define ,make 
         (let ((,new-obj (copy ,new-type)))
	   ,@(map (lambda (slot)
		    `(set! (,new-obj ',slot) ,slot))
		  (cdr make))
	   ,new-obj))
       
       ,@(map
	  (lambda (field)
	    (when (pair? field)
	      (if (null? (cdr field))
		  (values)
		  (if (null? (cddr field))
		      `(define (,(cadr field) ,obj)
			 (if (not (,? ,obj)) 
			     (error 'wrong-type-arg "~S should be a ~A" ,obj ',type))
			 (,obj ',(car field)))
		      `(begin
			 (define (,(cadr field) ,obj)
			   (if (not (,? ,obj)) 
			       (error 'wrong-type-arg "~S should be a ~A" ,obj ',type))
			   (,obj ',(car field)))
			 (define (,(caddr field) ,obj val)
			   (if (not (,? ,obj)) 
			       (error 'wrong-type-arg "~S should be a ~A" ,obj ',type))
			   (set! (,obj ',(car field)) val)))))))
	  fields)
       ',new-type)))

;;; srfi 111:
(define-record-type box-type (box value) box? (value unbox set-box!))


