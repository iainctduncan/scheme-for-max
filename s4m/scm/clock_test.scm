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


;; example using itm listen, tick is total tick count according to global transport
(define (cb-tick tick) 
  ;(post "(cb-tick) cur_ticks:" tick)
  ;; do a bang on quarter notes
  (if (= 0 (modulo tick 480)) 
    (out 0 :bang))
) 

;(listen-ticks 10 cb-tick)

(define (cb-ms time-ms) 
  (post "(cb-ms) curr_time:" time-ms)
  ;; send out a bang
  ;(out 0 :bang)
  #f
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
