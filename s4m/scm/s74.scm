;; s74.scm - s74 is a convenience layer over s7 with various
;; scheme functions ported from other implementations such as Racket, Clojure, R6RS, etc
;; Racket's dec and inc functions

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


;; test code for the dict getter
;; leaving for reference
;(define (hash-table-get ht . keys)
;  (define (getter container keylist)
;    (cond 
;      ((or (null? (cdr keylist)) (eq? #f (container (car keylist))))
;        (container (car keylist)))
;      (else (getter (container (car keylist)) (cdr keylist)))))
;  (getter ht keys))

;; example of an error try catch
;;(catch #t (lambda () (/ 1.0 0)) (lambda args (post :foobar)))


