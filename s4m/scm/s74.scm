;; s74.scm - s74 is a convenience layer over s7 with various
;; scheme functions ported from other implementations such as Racket, Clojure, R6RS, etc
;; Racket's dec and inc functions

; save some parens
(define (not-null? expr)
  (not (null? expr)))

; return arg incremented by
(define (inc arg)
  (+ 1 arg))

; return arg decremented by 1
(define (dec arg) 
  (- arg 1))

;; return a list of integers from start to finish (doesn't do skipping, yet)
;; not the best implementation, but will do for now
(define (range start end)
  (reverse 
    (let loop ((res (list start)))  
      (cond   
        ((= (- end 1) (car res)) res)
        (else (loop (cons (+ 1 (car res)) res)))))))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

;; looks like this is busted
(define (filter-out members lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) members) (filter-out members (cdr lat)))
    (else (cons (car lat) (filter-out members (cdr lat))))))

(define (filter predicate? lat)
  ;; filter out a list with a predicate
  (cond
    ((null? lat) '())
    ((not (predicate? (car lat))) (filter predicate? (cdr lat)))
    (else (cons (car lat) (filter predicate? (cdr lat))))))

;; MIT scheme's list-copy
(define (list-copy list)
  (if (null? list)
      '()
      (cons (car list)
            (list-copy (cdr list)))))

;; hash-table helpers
(define (hash-table-keys ht)
  (map (lambda (pair) (car pair)) ht))

(define (hash-table-values ht)
  (map (lambda (pair) (cdr pair)) ht))


; A threading/pipe operator similar to Clojure and R
; Takes an object as a starting value, then executes every form thereafter 
; with the value of one operation feeding into the next.
; You can use :$ (the hole marker) as the value of the previous operation, 
; You can also define this with a different hole marker if you would like,
; but :$ looks like Max's variable expansions while being safe.
(define-macro (~> object . functions)
  (let* ((hole :$)) ; you can redefine the hole symbol here if you want
    (define (plug-hole expression cork)
      (let iter ((expression expression))
        (cond
         ((null? expression) expression)
         ((eq? hole (car expression)) (cons cork
                                            (iter (cdr expression))))
         (else
          (cons (car expression) (iter (cdr expression)))))))
    (let thread-transform-loop
        ((needle object)
         (fns functions))
      (if (null? fns)
          needle
          (append (thread-transform-loop (plug-hole (car fns) needle)
                                         (cdr fns)))))))


;* some CM3 macros from Heinrich Taube, taken from the s7 test file
(define-macro (dolist spec . body)	;; spec = (var list . return)
	(let ((v (gensym)))
	  `(do ((,v ,(cadr spec) (cdr ,v))
		(,(car spec) #f))
	       ((null? ,v) ,@(cddr spec))
	     (set! ,(car spec) (car ,v))
	     ,@body)))

(define-macro (dotimes spec . body)	;; spec = (var end . return)
	(let ((e (gensym))
	      (n (car spec)))
	  `(do ((,e ,(cadr spec))
		(,n 0 (+ ,n 1)))
	       ((>= ,n ,e) ,@(cddr spec))
	     ,@body)))

(define-macro (do* spec end . body)
	`(let* (,@(map (lambda (var) (list (car var) (cadr var))) spec))
	   (do () ,end
	     ,@body
	     ,@(map (lambda (var) (if (pair? (cddr var))
				      `(set! ,(car var) ,(caddr var))
				      (values)))
		    spec))))

