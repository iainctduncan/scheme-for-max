;*=======================================================================*

(define (xy x y)
  (if (< x 0) (set! x (+ #x10000 x)))
  (if (< y 0) (set! y (+ #x10000 y)))
  (logior (ash (logand y #xffff) 16) (logand x #xffff) ))

(define (xy-x xy)
  (let ((x (logand xy #xffff)))
    (if (>= x #x8000) (- x #x10000) x)))

(define (xy-y xy)
  (let ((y (ash (logand xy #xffff0000) -16)))
    (if (>= y #x8000) (- y #x10000) y)))

(define (xy+ a b)
  (xy (+ (xy-x a) (xy-x b))
      (+ (xy-y a) (xy-y b))))

(define (automata-alloc cells rule index bounds gens window)
  (vector 'automata cells rule index bounds gens window))

(define (automata-cells a) (vector-ref a 1))
(define (automata-cells-set! a v) (vector-set! a 1 v))
(define (automata-rule a) (vector-ref a 2))
(define (automata-rule-set! a v) (vector-set! a 2 v))
(define (automata-index a) (vector-ref a 3))
(define (automata-index-set! a v) (vector-set! a 3 v))
(define (automata-bounds a) (vector-ref a 4))
(define (automata-bounds-set! a v) (vector-set! a 4 v))
(define (automata-generation a) (vector-ref a 5))
(define (automata-generation-set! a v) (vector-set! a 5 v))
(define (automata-window a) (vector-ref a 6))
(define (automata-window-set! a v) (vector-set! a 6 v))
(define (automata-dimensions a)
  (if (= (cadr (automata-bounds a)) 1) 1 2))
(define (automata? x)
  ;; return true if it smells like an automata
  (and (vector? x) 
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) 'automata)))

(define (make-automata cells rule . window)
  (let* ((auto (automata-alloc #f rule 0 #f 0 #f)) ;; allocate struct	 
         (array #f)
	 (r #f)
	 (c #f))
    (if (pair? cells)
	(cond ((pair? (car cells))
	       (set! r (length cells))
	       (set! c (length (car cells)))
	       (set! cells (if (= r 1) (car cells) cells))
	       ;; no generations on 2d automata
	       (automata-generation-set! auto #f)
	       )
	      (else
	       (set! r 1)
	       (set! c (length cells))
	       ))
	(error "cells not a list of states: ~S" cells))
    ;; three dimensional array holds the current and next states of
    ;; each index in the automata.  Dimension Z is always 2, where one
    ;; Z plane holds the current state of each cell and the other
    ;; plane holds the next state.
    (set! array (make-vector (list 2 r c) 0))
    (if (= r 1)
	(do ((x 0 (+ x 1)))
	    ((>= x c) #f)
	  (vector-set! array 0 0 x (list-ref cells x)))
	(do ((row cells (cdr row))
	     (y 0 (+ y 1)))
	    ((null? row) #f)
	  (do ((col (car row) (cdr col))
	       (x 0 (+ x 1)))
	      ((null? col) #f)
	    (vector-set! array 0 y x (car col)))))
    (automata-cells-set! auto array)
    (automata-bounds-set! auto (vector 0 r c))
    (if (not (null? window))
	(apply sw:open auto window))
    auto))

(define (cell-state auto index . inc)
  ;; return the current value of the automata at index in the
  ;; (current) Z plane of the state array
  (let ((p (if (null? inc) index (xy+ index (car inc))))
	(b (automata-bounds auto)))
    (vector-ref (automata-cells auto)
		(vector-ref b 0) ; access 'current state' plane (z plane)
		(modulo (xy-y p) (vector-ref b 1))
		(modulo (xy-x p) (vector-ref b 2)))))

(define-macro (with-states automata index neighbors . body)
  (let ((obj (gensym))
	(zyx (gensym))
	(ary (gensym)) 
        (ind (gensym))
	(row (gensym))
	(col (gensym)) 
        (tar (list #f)))

    (do ((tail neighbors (cdr tail))
	 (var tar) )
	((null? tail)
	 (set! tar (cdr tar)))
      (let* ((v (car (car tail)))
	     (p (cadr (car tail)))
	     (c (if (number? p)
		    (if (= (xy-x p) 0)
			col
			`(modulo (+ ,col ,(xy-x p)) (vector-ref ,zyx 2)))
		    `(modulo (+ ,col (xy-x ,p)) (vector-ref ,zyx 2))))
	     (r (if (number? p)
		    (if (= (xy-y p) 0) 
			row
			`(modulo (+ ,row ,(xy-y p)) (vector-ref ,zyx 1) ))
		    `(modulo (+ ,row (xy-y ,p)) (vector-ref ,zyx 1)))))
	(set-cdr! var
		  (list
		   `(,v (vector-ref ,ary (vector-ref ,zyx 0) ,r ,c))))
	(set! var (cdr var))))
    ;;(format #t "tar=~S~%" tar)
    `(let* ((,obj ,automata)
            (,ind ,index)
            (,row (xy-y ,ind))
            (,col (xy-x ,ind))
            (,zyx (automata-bounds ,obj))
            (,ary (automata-cells ,obj)))       
       
       (let ,tar ,@body))))
   
(define (state auto . all)
  (define (readstate auto window)
    (let* ((i (automata-index auto))
	   (x (xy-x i))
	   (y (xy-y i))
	   (b (automata-bounds auto))
	   (c (automata-cells auto))
	   (g (automata-generation auto)) ; #f if 2D
	   (curr #f)
	   (next #f)
	   )	
      ;; read current state at [z, y, x] 
      (set! curr (vector-ref c (vector-ref b 0) y x))
      ;; calculate the next state
      (set! next ( (automata-rule auto) auto i))
      ;; set next state at [(z+1 MOD 2), y, x]
      (vector-set! c (if (= (vector-ref b 0) 1) 0 1) y x next)
      ;; increment x y x MOD size. if at end increment Z.
      (set! x (+ x 1))
      (if (not (< x (vector-ref b 2)))
	  (begin (set! x 0)
		 (set! y (+ y 1))
		 (if (not (< y (vector-ref b 1)))
		     (let () 
		       (set! y 0)
		       (vector-set! b 0 (if (= (vector-ref b 0) 1) 0 1))
		       (if g (automata-generation-set! auto (+ g 1)))
		       ))))
      (automata-index-set! auto (xy x y))
      (if window
          (ffi_sw_draw window curr (or g (xy-y i)) (xy-x i)) ;;(sw:draw window curr (xy-y i) (xy-x i))
          )
      curr)) 
  ;; if all is true return a list of all states
  (if (or (null? all) (not (car all)))
      (readstate auto (automata-window auto))
      (let* ((b (automata-bounds auto))
	     (l (* (vector-ref b 1) (vector-ref b 2)))
	     (h (list #f))
	     (t h)
             (g (automata-generation auto)) ;; #f if 2D
	     (w (automata-window auto))
	     )
	(do ((i 0 (+ i 1)))
	    ((not (< i l))
	     (set! h (cdr h)))
	  (set-cdr! t (list (readstate auto #f)))
	  (set! t (cdr t)))
	(if (and w (> l 0))
	    ;(sw:draw w h l (or g 0) 0)
            (ffi_sw_draw w h l (or g 0))
            )
	h)))

;;;
;;; state window
;;;
                
(define (sw:open thing . args)
  (with-optkeys (args window colormap rows columns (cellsize 50)
		      (cellbordersize 1) (backgroundcolor "white"))
    (if (or (not (string? window))
	    (equal? window ""))
	(error "window not a string: ~S" window))
    (let ((statesandcolors #f))
      (if (not (pair? colormap))
	  (error "colormap is not a list of states and colors: ~S"
		 colormap))
      (do ((tail colormap (cddr tail)))
	  ((null? tail) #f)
	(if (null? (cdr tail))
	    (error "colormap is not a paired list of states and colors: ~S"
		   colormap))
	(let ((state (car tail))
	      (color (cadr tail)))
	  (if (not (integer? state))
	      (error "found non-integer state in colormap: ~S" state))
	  (if (symbol? color)
	      (set! color (symbol->string color)))
	  (if (not (member color *colors*))
	      (error "found non-color in colormap: ~A" color))
	  (if (not statesandcolors)
	      (set! statesandcolors (format #f "~S ~A" state color))
	      (set! statesandcolors 
		    (string-append statesandcolors 
				   (format #f " ~S ~A" state color))))))
      (if (symbol? backgroundcolor)
	  (set! backgroundcolor (symbol->string backgroundcolor)))
      (if (not (member backgroundcolor *colors*))
	  (error "backgroundcolor not a color: ~A" color))
      (cond ((automata? thing)
	     (let ((dims (vector-dimensions (automata-cells thing))))
               (if (= (cadr dims) 1) ; 1D automata
                   (set! rows (or rows (cadr dims)))
                   (set! rows (cadr dims))) ;; arraysize of y dimension
	       (set! columns (caddr dims)))) ;; arraysize of x dimension
	    (else
	     (error "cannot open state window for: ~S" thing)))
      (if (not (and (integer? rows) (> rows 0)))
	  (error "rows not an integer > 0: ~S" rows))
      (if (not (and (integer? columns) (> columns 0)))
	  (error "columns not an integer > 0: ~S" columns))      
      (if (not (and (integer? cellsize) (> cellsize 0)))
	  (error "cellsize not an integer > 0: ~S" cellsize))
      (if (not (and (integer? cellbordersize) (>= cellbordersize 0)))
	  (error "cellbordersize not an integer > 0: ~S" cellbordersize))
      (let ((xml 
	     (format #f "<statewindow title=~S colormap=~S rows=\"~D\" columns=\"~D\" cellsize=\"~D\" cellbordersize=\"~D\" backgroundcolor=~S/>"
		     window statesandcolors rows columns cellsize
		     cellbordersize backgroundcolor)))
	(if (ffi_sw_open_from_xml xml)
	    (begin
	      (automata-window-set! thing window)
	      )
	    (error "a window titled ~S already exists." window))
	(void)
	))))

(define (sw:draw window thing row col)
  (cond ((integer? thing)
	 (if (null? args)
	     (error "missing cell index for state: ~S" thing)
	     (let ()
	       (if (integer? index)
		   (ffi_sw_draw window thing row col)
		   (error "non-integer cell index: ~S" index)))))
	((pair? thing)
	 (ffi_sw_draw window thing (length thing) row))))

