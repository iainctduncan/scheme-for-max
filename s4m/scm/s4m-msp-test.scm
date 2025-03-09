(post "s4m-msp-test.scm loaded")

(define *vol* 1.0)


; something weird going on that without the post message we get a crash where the first vector is empty
(define (perform inL inR)
  (post "perform")
  ;(post "  - inL" inL)
  ;(post "  - inR" inR)
  (let* ((outL (make-vector (length inL) 0.0))
         (outR (make-vector (length inR) 0.0)))
    (do ((i 0 (+ 1 i))) ((>= i (length inR)))
      (set! (outL i) (* *vol* (inL i)))
      (set! (outR i) (* *vol* (inR i)))
      )
    ; return a list of numchans and chans vectors
    (list outL outR)))

