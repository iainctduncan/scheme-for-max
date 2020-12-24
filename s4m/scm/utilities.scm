;;; **********************************************************************
;;; Copyright (c) 2008, 2009 Rick Taube.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

;;;
;;; non-r5rs utilities that are not scheme implementation specific
;;;

(define (quit) 
  (ffi_quit))

; moved to c
;(define (cm-version )  (ffi_version ))
;(define (cm-logo ) (ffi_print_output (ffi_logo )))

(define (print-error msg)
  (ffi_print_error msg))

(define (print-warning msg)
  (ffi_print_warning msg))

(define (print-output msg)
  (ffi_print_output msg))

(define (print-values msg )
  (ffi_print_value msg))

(define (print-stdout msg )
  (ffi_print_stdout msg))

(define (print . args)
  (ffi_cm_print args)
  +s7-error+ ;; signal no value printing
  )

(define (shell str)
  (ffi_shell str))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define fifth (lambda (l) (car (cddddr l))))
(define sixth (lambda (l) (cadr (cddddr l))))
(define seventh (lambda (l) (caddr (cddddr l))))
(define eighth (lambda (l) (cadddr (cddddr l))))
(define ninth (lambda (l) (car (cddddr (cddddr l)))))
(define tenth (lambda (l) (cadr (cddddr (cddddr l)))))
(define nth list-ref)

(define (rest l) (cdr l))

; this is not cltl's last!
(define (last l) (if (null? (cdr l)) (car l) (last (cdr l))))

(define (butlast l)
  (cond ((null? (cdr l)) (list))
	((null? (cddr l)) (list (car l)))
	(else
	 (cons (car l) (butlast (cdr l))))))

(define (tail l) (if (null? (cdr l)) l (tail (cdr l))))

(define (unique l)
  (if (null? l) (list)
      (let* ((head (list #f))
	     (tail head))
	(do ((cdrs l (cdr cdrs)))
	    ((null? cdrs) (cdr head))
	  (if (not ( member (car cdrs) (cdr head)))
	      (begin (set-cdr! tail (list (car cdrs)))
		     (set! tail (cdr tail))))))))

; joins args into a list, args can be atoms or lists
(define (concat . args)
  (let* ((head (list #f))
	 (tail head))
    (do ((a args (cdr a)))
	((null? a) 
	 (cdr head))
      (cond ((null? (car a))
	     )
	    ((pair? (car a))
	     (do ((t (car a) (cdr t)))
		 ((null? t) #f)
	       (set-cdr! tail (list (car t)))
	       (set! tail (cdr tail))))
	    (else
	     (set-cdr! tail (list (car a)))
	     (set! tail (cdr tail)))))))

(define (reverse! l)
  (let ((s (length l)))
    (do ((i 0 (+ i 1))
	 (j (- s 1) (- j 1))
	 (t l (cdr t)))
	((not (< i j)) l)
      (let ((x (car t))
	    (z (list-tail t (- j i ))))
	(set-car! t (car z))
	(set-car! z x)))))

;(define (make-list n . obj)
;  (if (< n 0)
;      (error "~S is not a valid length" n))
;      (letrec ((l1 (lambda (a b)
;		     (if (= a 0) (list)
;			 (cons b (l1 (- a 1) b)))))
;	       (l2 (lambda (a b)
;		     ;; b is (<func> . args)
;		     (if (= a 0) (list)
;			 (cons ( apply (car b) (cdr b) )
;			       (l2 (- a 1) b))))))
;	(if (null? obj)
;	    (l1 n #f)
;	    (if (procedure? (car obj))
;		(l2 n obj)
;		(l1 n (car obj))))))

(define (list-index p l)
  (do ((tail l (cdr tail))
       (i 0  (+ i 1))
       (f #f))
      ((or f (null? tail ))
       f)
    (if ( p (car tail)) (set! f i))))

(define (list-find p l)
  (do ((tail l (cdr tail))
       (x #f)
       (f #f))
      ((or f (null? tail ))
       f)
    (if ( p (car tail)) (set! f (car tail)))))

(define (every? f l)
  (do ((tail l (cdr tail))
       (flag #t))
      ((or (not flag) (null? tail))
       flag)
    (if (f (car tail)) #f (set! flag #f))))

(define (list-prop lis prop . def)
  (if (null? lis)
      (if (null? def) #f (car def))
      (if (eqv? (car lis) prop)
          (cadr lis)
          (apply list-prop (cddr lis) prop def))))

(define (list-prop-set! lis prop val)
  (if (eqv? (car lis) prop)
      (set-car! (cdr lis) val)
      (if (null? (cddr lis))
          (set-cdr! (cdr lis) (list prop val))
          (list-prop-set! (cddr lis) prop val))))

;; sequence mapping.

(define (%mapseq func seq elements? start end)
  ;; this is the workhorse, if elements is true it returns elements
  ;; else positions
  (let ((seqlen #f)
	(getter #f))
    (cond ((list? seq)
	   (set! getter list-ref)
	   (set! seqlen length))
	  ((string? seq)
	   (set! getter string-ref)
	   (set! seqlen string-length))
	  ((vector? seq)
	   (set! getter vector-ref)
	   (set! seqlen vector-length))
	  (else
	   (error "~S is not a list, string or vector" seq)))
    (do ((i start (+ i 1))
	 (l (or end (seqlen seq)))
	 (e #f)
	 (f #f)
	 (r #f))
	((or (>= i l) f)
	 r)
      (set! e (getter seq i))
      (if (func e)
	  (begin (set! r (if elements? e i))
		 (set! f #t))))))

#|
(let ((args '(1 2 3)))  (with-optkeys (args a b c) (list a b c)))
(let ((args '(1 2 3 4)))  (with-optkeys (args a b c) (list a b c))) ; too many args error
(let ((args '(1 2))) (with-optkeys (args a b (c 33)) (list a b c)))
(let ((args '())) (with-optkeys (args a b (c 33)) (list a b c)))
(let ((args '(:b 22))) (with-optkeys (args a b (c 33)) (list a b c)))
(let ((args '(-1 :z 22))) (with-optkeys (args a b (c 33)) (list a b c))) ; :z is positional
(let ((args '(:b 99 :z 22))) (with-optkeys (args a b (c 33)) (list a b c))) ; :z is bad key
(let ((args '(:z 22))) (with-optkeys (args a b (c 33) &allow-other-keys) (list a b c &allow-other-keys)))
(let ((args '(:id "0" :inst "flute" :name "Flute"))) (with-optkeys (args id inst &allow-other-keys) (list id inst &allow-other-keys)))
(let ((args '(:inst "flute" :id "0" :name "Flute"))) (with-optkeys (args id inst &allow-other-keys) (list id inst &allow-other-keys)))
(let ((args '(:id "0" :name "Flute" :inst "flute"))) (with-optkeys (args id inst &allow-other-keys) (list id inst &allow-other-keys)))
(let ((args '(:name "Flute" :inst "flute" :id "0"))) (with-optkeys (args id inst &allow-other-keys) (list id inst &allow-other-keys)))
|#

;(define-macro (with-optkeys spec . body)
;  (expand-optkeys (car spec) (cdr spec) body))

(define-expansion (with-optkeys spec . body)
  (
   (lambda (user rawspec body)

     (define (key-parse-clause info mode args argn user)
       ;; return a cond clause that parses one keyword. info for each
       ;; var is: (<got> <var> <val>)
       (let* ((got (car info))
	      (var (cadr info))
	      (key (string->keyword (symbol->string var))))
	 `((eq? (car ,args) ,key )
	   (if ,got (error "duplicate keyword: ~S" , key))
	   (set! ,var (if (null? (cdr ,args))
			  (error "missing value for keyword: ~S" 
				 , user)
			  (cadr ,args)))
	   (set! ,got #t) ; mark that we have a value for this param
	   (set! ,mode #t) ; mark that we are now parsing keywords
           (set! ,argn (+ ,argn 1))
	   (set! ,args (cddr ,args)))))

     (define (pos-parse-clause info mode args argn I)
       ;; return a cond clause that parses one positional. info for
       ;; each var is: (<got> <var> <val>)
       (let ((got (car info))
	     (var (cadr info)))
         `((= ,argn ,I)
           (set! ,var (car ,args))
           (set! ,got #t) ; mark that we have a value for this param
           (set! ,argn (+ ,argn 1))
           (set! ,args (cdr ,args)))))
     
     (let* ((otherkeys? (member '&allow-other-keys rawspec))
            ;; remove &allow-other-keys from spec
	    (spec (if otherkeys? (reverse (cdr (reverse rawspec))) rawspec))
	    (data (map (lambda (v)
			 ;; for each optkey variable v return a list
			 ;; (<got> <var> <val>) where the <got>
			 ;; variable indicates that <var> has been
			 ;; set, <var> is the optkey variable itself
			 ;; and <val> is its default value
			 (if (pair? v)
			     (cons (gensym (symbol->string (car v))) v)
			     (list (gensym (symbol->string v)) v #f)))
		       spec))
	    (args (gensym "args")) ; holds arg data as its parsed
            (argn (gensym "argn"))
            (SIZE (length data))
	    (mode (gensym "keyp")) ; true if parsing keywords
	    ;; keyc are cond clauses that parse valid keyword
	    (keyc (map (lambda (d) (key-parse-clause d mode args argn user))
		       data))
            (posc (let lup ((tail data) (I 0))
                    (if (null? tail) (list)
                        (cons (pos-parse-clause (car tail) mode args argn I)
                              (lup (cdr tail) (+ I 1))))))
	    (bindings (map cdr data)) ; optkey variable bindings
	    )

       (if otherkeys?
           (set! bindings (cons '(&allow-other-keys (list)) bindings)))
      
       `(let* ,bindings ; bind all the optkey variables with default values
	  ;; bind status and parsing vars
	  (let ,(append (map (lambda (i) (list (car i) #f)) data)
			`((,args ,user)
                          (,argn 0)
			  (,mode #f)))
            ;; iterate arglist and set opt/key values
            (do ()
                ((null? ,args) #f)
              (cond 
               ;; add valid keyword clauses first
               ,@ keyc
               ;; a keyword in (car args) is now either added to
               ;; &allow-other-keys or an error
               , (if otherkeys?
                     `((keyword? (car ,args))
                       (if (not (pair? (cdr ,args)))
                           (error "missing value for keyword ~S" (car ,args)))
                       (set! &allow-other-keys (append &allow-other-keys
                                                       (list (car ,args)
                                                             (cadr ,args))))
                       (set! ,mode #t) ; parsing keys now...
                       (set! ,args (cddr ,args)) )
                     `((keyword? (car ,args)) ;(and ,mode (keyword? (car ,args)))
                       (error "invalid keyword: ~S" (car ,args)) )
                     )
                 ;; positional clauses illegal if keywords have happened
                 (,mode (error "positional after keywords: ~S" (car ,args)))
                 ;; too many value specified
                 ((not (< ,argn ,SIZE)) (error "too many args: ~S" , args))
                 ;; add the valid positional clauses
                 ,@ posc
              ))
            ,@ body))
       ))
   (car spec)
   (cdr spec)
   body
   ))

#|

;; OLD VERSION

(define-expansion (with-optkeys spec . body)
  ;(expand-optkeys user rawspec body)
  (
   (lambda (user rawspec body)
     ;;(format #t "in expand-optkeys~%")
     (define (key-parse-clause info mode args user)
       ;; return a case clause that parses one keyword
       ;; info for each var: (<got> <var> <val>)
       (let* ((got (car info))
	      (var (cadr info))
	      (key (string->keyword (symbol->string var)))
	      )
	 `(( ,key )
	   (if ,got (error "~S is a redundant keyword" , key))
	   (set! ,var (if (null? (cdr ,args))
			  (error "missing keyword value in ~S" 
				 , user)
			  (cadr ,args)))
	   (set! ,got #t) ; mark that we have a value for this param
	   (set! ,mode #t) ; mark that we are now parsing keywords
	   (set! ,args (cddr ,args)))))
     (define (opt-parse-clause info mode pars user otherkeys?)
       (let ((got (car info))
	     (var (cadr info)))
	 (if otherkeys?
	     `(else ; &allow-other-keys handing
	       (if (and (keyword? (car ,pars)) (pair? (cdr ,pars)))
		   (begin
		     (set! &allow-other-keys (append &allow-other-keys
						     (list (car ,pars)
							   (cadr ,pars))))
		     (set! ,pars (cddr ,pars)))
		   (begin
		     (if ,mode (error "found positional argument after keyword in ~S" 
				      ,user))
		     (set! ,var (car ,pars))
		     (set! ,got #t) ; mark that we have a value for this param
		     (set! ,pars (cdr ,pars)))))
	     `(else
	       (if ,mode (error "found positional argument after keyword in ~S" 
				,user))
	       (set! ,var (car ,pars))
	       (if (keyword? ,var)
		   (error "~S is not a valid keyword",  var))
	       (set! ,got #t) ; mark that we have a value for this param
	       (set! ,pars (cdr ,pars))))))
     (define (parse-optkey info data mode args user keyc otherkeys?)
       ;; return a complete parsing clause for one optkey variable. keyc
       ;; holds all the key parsing clauses used by each case statement
       `(if (not (null? ,args))
	    (case (car ,args)
	      ;; generate all keyword clauses
	      ,@ keyc
		 , (opt-parse-clause info mode args user otherkeys?))))
     
     (let* ((otherkeys? (member '&allow-other-keys rawspec))
	    (spec (if otherkeys? (reverse (cdr (reverse rawspec))) rawspec))
	    (data (map (lambda (v)
			 ;; for each optkey variable v return a list
			 ;; (<got> <var> <val>) where the <got> variable
			 ;; indicates that <var> has been set, <var> is
			 ;; the optkey variable and <val> is its default
			 ;; value
			 (if (pair? v)
			     (cons (gensym (symbol->string (car v))) v)
			     (list (gensym (symbol->string v)) v #f)))
		       spec))
	    (args (gensym "args")) ; holds arg data as its parsed
	    (mode (gensym "keyp")) ; true if parsing keywords
	    ;; the case clauses parsing each keyword
	    (keyc (map (lambda (i) (key-parse-clause i mode args user))
		       data))
	    (terminal #f)  ; clause executed at end
	    (bindings (map cdr data)) ; list of variable bindings for 
	    )
       ;; termination clause either looks for dangling values or sets
       ;; &allow-other-keys to the remaining args (if they are keyword)
       (if (not otherkeys?)
	   (set! terminal
		 `(if (not (null? ,args))
		      (error "too many arguments in ~S" , user)))
	   (let ((check (gensym "others")))
	     (set! bindings (cons '(&allow-other-keys (list)) bindings))
	     (set! terminal
		   `(do ((,check ,args (cddr ,check)))
			((null? ,check)
			 (set! &allow-other-keys
			       (append &allow-other-keys ,args)))
		      (if (not (keyword? (car ,check)))
			  (error "~S is not a keyword" (car ,check))
			  (if (null? (cdr ,check))
			      (error "missing value for ~S"
				     (car ,check))))))))
       `(let* , bindings ; bind all the optkey variables with default values
	  ;; bind status and parsing vars
	  (let ,(append (map (lambda (i) (list (car i) #f)) data)
			`((,args ,user)
			  (,mode #f)))
	    ;; generate a parsing expression for each optkey variable
	    ,@ (map (lambda (i)
		      (parse-optkey i data mode args user keyc otherkeys?))
		    data)
	       ;; add terminal check to make sure no dangling args or add
	       ;; extra keywords to &allow-other-keys
	       ,terminal
	       ,@ body))))
   ;; EXPANSION ARGS
   (car spec) (cdr spec) body
   ))
|#

; (pretty-print (expand-optkeys 'args '(a (b 2)) '((list a b))))
; (pretty-print (expand-optkeys 'args '(a (b 2) &allow-other-keys) '((list a b  &allow-other-key))))
; (define (test . args) (with-optkeys (args a (b 2)) (list a b)))
; (define (test2 . args) (with-optkeys (args a (b 2) &allow-other-keys) (list a b &allow-other-keys)))
; (test 1)
; (test #:a 1)
; (test #:a 1 #:zz)
; (test2 #:a 1 #:zz 222 #:ppop 111)

(define (find x seq . args)
  (with-optkeys (args (key #f) (start 0) (end #f))
    (%mapseq (if key
		 (lambda (a) (equal? (key a) x))
		 (lambda (a) (equal? a x)))
	     seq
	     #t
	     start
	     end)))

(define (position x seq . args)
  (with-optkeys (args (key #f) (start 0) (end #f))
    (%mapseq (if key
		 (lambda (a) (equal? (key a) x))
		 (lambda (a) (equal? a x)))
	     seq
	     #f
	     start
	     end)))

(define (string-hash str)
  (if (not (or (string? str) (number? str)))
    (error "paramter ~S is not a string or a number" str))
  (if (string? str)
    (set! str (ffi_string_hash str)))
  str)
