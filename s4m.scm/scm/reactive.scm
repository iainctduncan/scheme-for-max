;;; reactive.scm
;;;
;;; reimplementation of code formerly in stuff.scm

(provide 'reactive.scm)
;(set! (*s7* 'gc-stats) #t)

(define (symbol->let symbol env)
  ;(format *stderr* "symbol->let ~S~%" symbol)
  ;; return let in which symbol lives (not necessarily curlet)
  (if (defined? symbol env #t)
      env	
      (if (eq? env (rootlet))
	  #<undefined>
	  (symbol->let symbol (outlet env)))))

(define (gather-symbols expr ce lst ignore)
  ;; collect settable variables in expr
  (cond ((symbol? expr)
	 (if (or (memq expr lst)
		 (memq expr ignore)
		 (procedure? (symbol->value expr ce))
		 (eq? (symbol->let expr ce) (rootlet)))
	     lst
	     (cons expr lst)))

	((not (pair? expr)) lst)

	((not (and (pair? (cdr expr)) (pair? (cddr expr))))
	 (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore))

	((pair? (cadr expr))
	 (gather-symbols (case (car expr)
			   ((let let* letrec letrec* do)
			    (values (cddr expr) ce lst (append ignore (map car (cadr expr)))))
			   ((lambda) 
			    (values (cddr expr) ce lst (append ignore (cadr expr))))
			   ((lambda*)
			    (values (cddr expr) ce lst (append ignore (map (lambda (a) (if (pair? a) (car a) a)) (cadr expr)))))
			   (else
			    (values (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore)))))

	((and (eq? (car expr) 'lambda)
	      (symbol? (cadr expr)))
	 (gather-symbols (cddr expr) ce lst (append ignore (list (cadr expr)))))

	(else 
	 (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore))))


;;; c-pointer used to hold symbol+let info so that the lets can be a "weak references"
(define slot-symbol c-pointer-type)
(define slot-expr c-pointer-info)
(define slot-env c-pointer-weak1)
(define slot-expr-env c-pointer-weak2)
(define (slot symbol expr env expr-env) (c-pointer 0 symbol expr env expr-env))

(define (setter-update cp)            ; cp: (slot var expr env expr-env)
  ;; when var set, all other vars dependent on it need to be set also, watching out for GC'd followers
  (when (and (let? (slot-env cp))
	     (let? (slot-expr-env cp)))     ; when slot-env is GC'd, the c-pointer field is set to #f (by the GC)
    (let ((val (eval (slot-expr cp) (slot-expr-env cp))))
      (when (let? (slot-env cp))      ; same as above, but eval may trigger gc
	(let-set! (slot-env cp)
		  (slot-symbol cp)
		  val)))))


(define (slot-equal? cp1 cp2)
  (and (eq? (slot-symbol cp1) (slot-symbol cp2))
       (eq? (slot-env cp1) (slot-env cp2))))

(define (setter-remove cp lst)
  ;; if reactive-set! called again on a variable, its old setters need to remove the now obsolete set of that variable
  (map (lambda (c)
	 (if (slot-equal? cp c)
	     (values)
	     c))
       lst))


(define* (make-setter var env (followers ()) (setters ()) (expr ()) expr-env)
  ;; return a new setter with closure containing the followers and setters of var, and the c-pointer holding its name, environment, and expression
  (let ((followers followers)
	(setters setters)
	(cp (slot var expr env expr-env)))
    (lambda (sym val)
      ;(format *stderr* "make-setter ~S ~S~%" sym val)
      (let-temporarily (((setter (slot-symbol cp) (slot-env cp)) #f))
			 ;(setter (c-pointer-type cp) (c-pointer-weak1 cp)) #f))
	(let-set! (slot-env cp) (slot-symbol cp) val) ; set new value without retriggering the setter
	(for-each setter-update followers)            ; set any variables dependent on var
	val))))

(define (update-setters setters cp e)
  ;; add the slot to the followers setter list of each variable in expr
  (for-each (lambda (s)
	      (unless (and (setter s e)
			   (defined? 'followers (funclet (setter s e))))
		(set! (setter s e) (make-setter s e)))
	      (let ((setter-followers (let-ref (funclet (setter s e)) 'followers)))
		(unless (member cp setter-followers slot-equal?)
		  (let-set! (funclet (setter s e))
			    'followers
			    (cons cp setter-followers)))))
	    setters))

(define (clean-up-setter old-setter old-followers lt place e)
  ;; if previous set expr, remove it from setters' followers lists
  (when (and old-setter
	     (defined? 'followers (funclet old-setter))
	     (defined? 'setters (funclet old-setter)))
    (set! old-followers ((funclet old-setter) 'followers))
    (for-each (lambda (s)
		(when (and (setter s e)
			   (defined? 'followers (funclet (setter s e))))
		  (let ((setter-followers (let-ref (funclet (setter s e)) 'followers)))
		    (let-set! (funclet (setter s e))
			      'followers 
			      (setter-remove (slot place 0 lt e) setter-followers)))))
	      (let-ref (funclet old-setter) 'setters)))
  old-followers)

(define-bacro (reactive-set! place value)
  (with-let (inlet 'place place                       ; with-let here gives us control over the names
		   'value value 
		   'e (outlet (curlet)))              ; the run-time (calling) environment
    `(let ((old-followers ())
	   (old-setter (setter ',place))
	   (lt (symbol->let ',place ,e)))
       (set! old-followers (clean-up-setter old-setter old-followers lt ',place ,e))
       ;; set up new setter
       (let ((setters (gather-symbols ',value ,e () ())))
	 (when (pair? setters)
	   (let ((expr (if (pair? ',value) (copy ',value :readable) ',value)))
	     (let ((cp (slot ',place expr lt ,e)))
	       (set! (setter ',place lt)
		     (make-setter ',place lt old-followers setters expr ,e))
	       (update-setters setters cp ,e)))))
       (set! ,place ,value))))



;; --------------------------------------------------------------------------------
#|
(let ()
(define a 2)
(define b 1)
(define x 0)
(reactive-set! x (+ a b))

(set! a 3)
(format *stderr* "x: ~A~%" x)
(set! b 4)
(format *stderr* "x: ~A~%" x)

(format *stderr* "x setter: ~S ~S~%" (setter 'x) (funclet (setter 'x)))
(format *stderr* "a setter: ~S ~S~%" (setter 'a) (funclet (setter 'a)))
;; x setter: #<lambda (sym val)> (inlet 'followers () 'setters (b a) 'cp #<x (nil)>)
;; a setter: #<lambda (sym val)> (inlet 'followers (#<x (nil)>) 'setters () 'cp #<a (nil)>)

(reactive-set! a (* b 2))
(set! b 5)
(format *stderr* "x: ~A, a: ~A, b: ~A~%" x a b)
;; x: 15, a: 10, b: 5
)

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! x (* 2 a)))
    (let ((a 3))
      (set! a 2))
    (if (zero? (modulo i 10))
	(gc))))

(define-macro (test a b)
  ;(display a) (newline)
  `(if (not (equal? ,a ,b))
       (format *stderr* "~S -> ~S?~%" ',a ,b)))


(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (+ b c)) (set! b 4) (set! c 5) a) 9)
(test (let ((a 1) (b 2) (c 3)) (reactive-set! b (+ c 4)) (reactive-set! a (+ b c)) (set! c 5) a) 14)
(test (let ((expr 21) (symbol 1)) (reactive-set! expr (* symbol 2)) (set! symbol 3) expr) 6)
(test (let ((a 21) (b 1)) (reactive-set! a (* b 2)) (set! b 3) a) 6)
(test (let ((s 21) (v 1)) (reactive-set! s (* v 2)) (set! v 3) s) 6)
(test (let ((a 21) (v 1)) (reactive-set! a (* v 2)) (set! v 3) a) 6)
(test (let ((symbol 21) (nv 1)) (reactive-set! symbol (* nv 2)) (set! nv 3) symbol) 6)
(test (let ((outer 0)) (let ((nv 21) (sym 1)) (let ((inner 1)) (reactive-set! nv (* sym 2)) (set! sym 3) nv))) 6)
(test (let ((a 1) (b 2)) (reactive-set! b (+ a 4)) (let ((a 10)) (set! a (+ b 5)) (list a b))) '(10 5))
(test (let ((a 1) (b 2)) (reactive-set! b (+ a 4)) (list (let ((b 10)) (set! a (+ b 5)) a) b)) '(15 19))

(test (let ((a 1) (b 2) (c 3)) (reactive-set! b (+ c 4)) (let ((a 0)) (reactive-set! a (+ b c)) (set! c 5) a)) 14)
(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (reactive-set! b (+ c 4))) (list a b c)) '(7 7 3))
(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (+ 1 (reactive-set! b (+ c 4)))) (list a b c)) '(8 7 3))

(test (let ((a 1) (x 0)) (reactive-set! x (* a 2)) (reactive-set! a (* x 2)) (set! x 2) a) 4)
(test (let ((a 1)) (let ((b 0) (c 0)) (reactive-set! b (* a 2)) (reactive-set! c (* a 3)) (let ((x 0)) (reactive-set! x (+ a b c)) (set! a 2) x))) 12)
(test (let ((x 0)) (let ((a 1)) (reactive-set! x (* 2 a)) (set! a 2)) x) 4)

(test (let ((x 0) (a 1)) (reactive-set! x (+ a 1)) (reactive-set! a (+ x 2)) (set! a 3) (set! x 4) (list x a)) (list 4 6))
(test (let ((x 0) (a 1) (b 0)) (reactive-set! x (+ a 2)) (let ((x 2)) (reactive-set! x (+ a 1)) (set! a 4) (set! b x)) (list x a b)) (list 6 4 5))
(test (let ((x 0)) (reactive-set! x (* 3 2)) x) 6)
(test (let ((x 0)) (reactive-set! x (* pi 2)) x) (* pi 2))
(test (let ((x 0)) (let ((a 1)) (reactive-set! x a) (set! a 2)) x) 2)

;;; (define-macro (with-setters vars . body) `(let-temporarily (,(map (lambda (var) `((setter ',var) #f)) vars)) ,@body))

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! a (* 2 x))
      (set! x 2)
      (if (zero? (modulo i 10))
	  (gc)))))

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! x (* 2 a))
      (set! a 2))))

(test (let ((a 21) (b 1)) (set! (setter 'b) (lambda (x y) (* 2 y))) (reactive-set! a (* b 2)) (set! b 3) a) 6) ; old setter ignored
(test (let ((a 21) (b 1)) (set! (setter 'b) (lambda (x y) (* 2 y))) (let ((b 2)) (reactive-set! a (* b 2)) (set! b 3) a)) 6)

;; also place as generalized set: (reactive-set! (v 0) (* a 2)) -- does v get the setter?
|#
;;; --------------------------------------------------------------------------------

(define-bacro (reactive-let vars/inits . body)
  (with-let (inlet 'vars/inits vars/inits 
		   'body body
		   'e (outlet (curlet)))
    (let ((vars (map car vars/inits))
	  (inits (map cadr vars/inits)))
      (let ((reacts (map (lambda (var init)
			   `(let ((setters (gather-symbols ',init ,e () ())))
			      (when (pair? setters)
				(let ((expr (if (pair? ',init) (copy ',init :readable) ',init))
				      (lt (curlet)))
				  (let ((cp (slot ',var expr lt ,e)))
				    (set! (setter ',var lt)
					  (make-setter ',var lt () setters expr ,e))
				    (for-each (lambda (s)
						(unless (and (setter s)
							     (defined? 'followers (funclet (setter s))))
						  (set! (setter s) (make-setter s lt)))
						(let ((setter-followers (let-ref (funclet (setter s)) 'followers)))
						  (unless (member cp setter-followers slot-equal?)
						    (let-set! (funclet (setter s))
							      'followers
							      (cons cp setter-followers)))))
					      setters))))))
			 vars inits)))
	(cons 'let (cons vars/inits (append reacts body)))))))


;;; --------------------------------------------------------------------------------
#|
  (test (reactive-let () 3) 3)
  (test (let ((a 1)) (reactive-let ((b (+ a 1))) b)) 2)
  (test (let ((a 1)) (+ (reactive-let ((b (+ a 1))) (set! a 3) b) a)) 7)
  (test (let ((a 1)) (+ (reactive-let ((b (+ a 1)) (a 0)) (set! a 3) b) a)) 3)
  (test (let ((a 1)) (reactive-let ((a 2) (b (* a 3))) (set! a 3) b)) 3)
  (test (let ((a 1) (b 2)) (reactive-let ((a (* b 2)) (b (* a 3))) (set! a 3) b)) 3)
  (test (let ((a 1) (b 2)) (reactive-let ((a (* b 2)) (b (* a 3))) (set! b 3) a)) 4)
  (test (let ((a 1) (b 2)) (reactive-let ((a (* b 2))) (set! b 3) a)) 6)
  (test (let ((a 1)) (reactive-let ((b (+ a 1))) (set! a 3) b)) 4)
  (test (let ((a 1)) (reactive-let ((b (+ a 1)) (c (* a 2))) (set! a 3) (+ c b))) 10)
  (test (let ((a 1) (d 2)) (reactive-let ((b (+ a d)) (c (* a d)) (d 0)) (set! a 3) (+ b c))) 11)
  (test (let ((a 1) (d 2)) (reactive-let ((b (+ a d)) (c (* a d)) (d 0)) (set! a 3)) (setter 'a)) #f)
  (test (let ((a 1) (d 2)) (reactive-let ((b (+ a d)) (c (* a d)) (d 0)) (set! a 3) (set! d 12) (+ b c))) 11)
  (test (let ((a 1) (b 2)) (+ (reactive-let ((b (+ a 1)) (c (* b 2))) (set! a 3) (+ b c)) a b)) 13)  ;c=4 because it watches the outer b
  (test (let ((a 1)) (reactive-let ((b (* a 2))) (reactive-let ((c (* a 3))) (set! a 2) (+ b c)))) 10)
  (test (let ((a 1)) (reactive-let ((b (* a 2))) (let ((d (reactive-let ((c (* a 3))) c))) (set! a 2) (+ b d)))) 7)
  (test (let ((a 1)) (reactive-let ((b (* a 2))) (+ (reactive-let ((c (* a 3))) c) (set! a 2) b))) 9) ; a=2 is added to b=4 and c=3
  (test (let ((a 1)) (reactive-let ((b (+ a 1))) (reactive-let ((c (* b 2))) (begin (set! a 3) (+ c b))))) 12)
  (test (reactive-let ((a (lambda (b) b))) (a 1)) 1)
  (test (reactive-let ((a (let ((b 1) (c 2)) (+ b c)))) a) 3)
  (test (let ((b 1)) (reactive-let ((a (let ((b 1) (c 2)) (+ b c))) (c (* b 2))) (set! b 43) c)) 86)
  (test (let ((x 0.0)) (reactive-let ((y (sin x))) (set! x 1.0) y)) (sin 1.0))
  (test (let ((a 1)) (reactive-let ((b a) (c a)) (set! a 3) (list b c))) '(3 3))
  (test (let ((a 1)) (reactive-let ((b a)) (reactive-let ((c (* b a))) (set! a 3) (list b c)))) '(3 9))
  (test (let ((a 1) (b 2)) (reactive-let ((c a) (d (* b a))) (set! a 3) (list a b c d))) '(3 2 3 6))
  (test (let ((a 1)) (reactive-let ((b (* a 2)) (c (* a 3)) (d (* a 4))) (set! a 2) (list a b c d))) '(2 4 6 8))
  (test (let ((b 2)) (reactive-let ((a (* b 2))) (+ (reactive-let ((a (* b 3))) (set! b 3) a) a))) 15)
|#
;;; --------------------------------------------------------------------------------

(define-macro (reactive-let* vars . body)
  (let add-let ((v vars))
    (if (pair? v)
	`(reactive-let ((,(caar v) ,(cadar v)))
	   ,(add-let (cdr v)))
	(cons 'begin body))))


;;; --------------------------------------------------------------------------------
#|
  (test (let ((a 1)) (reactive-let* ((b a) (c (* b a))) (set! a 3) (list b c))) '(3 9))
  (test (let ((a 1)) (reactive-let* ((b a) (x (+ a b))) (set! a 3) (list b x))) '(3 6))
  (test (let ((x 0.0)) (reactive-let* ((y x) (z (* y (cos x)))) (set! x 1.0) z)) (cos 1.0))
|#
;;; --------------------------------------------------------------------------------

#|
(let ()
  (define xyzzy (let ((x 0)) 
		  (dilambda 
		   (lambda () 
		     x) 
		   (lambda (val)
		     (set! x val)))))
  (let ((a 1)) 
    (reactive-set! (xyzzy) (+ a 1))
    (set! a 2)
    (xyzzy))

  (let ((a 1))
    (reactive-set! a (+ (xyzzy) 1))
    (set! (xyzzy) 2)
    a)

  (reactive-let ((a (+ (xyzzy) 1)))
    (set! (xyzzy) 2)
    a))

;;; not different?:

(let ((v (vector 1 2 3)))
  (let ((a 1))
    (reactive-set! (v 0) (+ a 1))
    (set! a 2)
    (v 0)))

;;; but where to place the setter in either case -- on 'a and save the location, but then how to erase if reset?
;;;   and how to ignore if xyzzy arg not the same?
;;; insist that (f) f be a thunk/dilambda, and in the (set! (f)...) case, put the setter on the setter? (set! (setter (setter f)) ...)


<p>Here's the standard example of following the mouse (assuming you're using Snd and glistener):
</p>
<pre class="indented">
(let ((*mouse-x* 0) (*mouse-y* 0)
      (x 0) (y 0))

  (reactive-set! x (let ((val (round *mouse-x*))) 
		     (format *stderr* "mouse: ~A ~A~%" x y) 
		     val))
  (reactive-set! y (round *mouse-y*))

  (g_signal_connect (G_OBJECT (listener-text-widget *listener*)) "motion_notify_event" 
		    (lambda (w e d) 
		      (let ((mxy (cdr (gdk_event_get_coords (GDK_EVENT e)))))
			(set! *mouse-x* (car mxy))
			(set! *mouse-y* (cadr mxy))))))
</pre>
|#
