(post "thread-reset-test-clocker.scm")


(define playing #f)
(define count 0)

(define (start)
  (post "starting")
  (set! playing #t)
  (clock)
)

(define (stop)
  (post "stopping")
  (set! playing #f)
)

(define (clock)
  (set! count (+ 1 count))
  (if (= 0 (modulo count 480))
    (post "clock count:" count)
    (out 0 count))

  (if playing
    (delay-t 1 clock))
)
