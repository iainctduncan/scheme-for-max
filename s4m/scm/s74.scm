;; s74.scm - s74 is a convenience layer over s7 with various
;; scheme functions ported from other implementations such as Racket, Clojure, R6RS, etc
;; Racket's dec and inc functions
(define (dec arg) 
  (- arg 1))

(define (inc arg)
  (+ 1 arg))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

(define (filter-out members lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) members) (filter-out members (cdr lat)))
    (else (cons (car lat) (filter-out members (cdr lat))))))

(define (filter predicate? lat)
  (cond
    ((null? lat) '())
    ((not (predicate? (car lat))) (filter predicate? (cdr lat)))
    (else (cons (car lat) (filter predicate? (cdr lat))))))
