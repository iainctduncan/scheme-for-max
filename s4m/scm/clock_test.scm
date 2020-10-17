(post "clock_test.scm")

(define playing #f)

(define (start)
  (set! playing #t)
  (delay-t 480 480 tick)
)

;; self scheduling example
(define (tick) 
  (post "(tick)")
  (out 0 :bang)
  (if playing 
    (delay-t '4n '4n tick))
) 


;; example using itm listen
(define (cb-tick cur_ticks) 
  (post "(cb-tick) cur_ticks:" cur_ticks)
  (out 0 :bang)
) 

;(listen-ticks 10 cb-tick)



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
