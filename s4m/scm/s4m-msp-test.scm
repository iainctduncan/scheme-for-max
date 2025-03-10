(post "s4m-msp-test.scm loaded")

(define *vol* 1.0)


; uses globals in-l, in-r, out-l, out-r, defined on c side
(define (perform frames)
  ;(post "perform new")
  ;(post "in-l" in-l)
  ;(post "in-r" in-r)
 
  (do ((i 0 (+ 1 i))) ((>= i frames))
    (set! (out-l i) 
      (* (in-l i) *vol*))
    (set! (out-r i) 
      (* (in-r i) *vol*))
    ))


