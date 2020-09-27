(post "clock_test.scm")

(define (f) (post "f running"))

;;(clock 2000.0 f)


(define (f-list arg)
  (post "f-list, arg:" arg))

(define (f-int arg)
  (post "f-int, arg:" arg))

(define (f-float arg)
  (post "f-float, arg:" arg))

(define (f-bang)
  (post "f-bang"))



(post "done clock_test.scm")
