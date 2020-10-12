(post "clock_test.scm")

(define playing #f)

(define (start)
  (set! playing #t)
  (delay-t 480 480 tick)
)


(define (tick) 
  (post "(tick)")
  (out 0 :bang)
  (if playing 
    (delay-t '4n '4n tick))
) 

;(delay-t 480 f)

(define (f-list arg)
  (post "f-list, arg:" arg))

(define (f-int arg)
  (post "f-int, arg:" arg))

(define (f-float arg)
  (post "f-float, arg:" arg))

(define (f-bang)
  (post "f-bang"))



(post "done clock_test.scm")
