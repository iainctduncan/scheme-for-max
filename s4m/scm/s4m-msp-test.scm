(post "s4m-msp-test.scm loaded")

(define *vol* 1.0)

(define (perform inL inR)
  (let* ((outL (make-vector (length inL) 0.0))
         (outR (make-vector (length inR) 0.0)))
    (do ((i 0 (+ 1 i))) ((>= i (length inL)))
      (set! (outL i) (* *vol* (inL i)))
      (set! (outR i) (* *vol* (inR i)))
      )
    ; return a list of numchans and chans vectors
    (list outL outR)))

