(post "s4m-msp-test.scm loaded")

(define *vol* 1.0)

(define (perform chans . args) 
  (post "(perform) chans:", chans)
  (if (= chans 1)
    (perform-mono   (args 0))
    (perform-stereo (args 0) (args 1))))

(define (perform-mono inbuf)
  (let* ((outbuf (make-vector (length inL) 0.0)))
    (do ((i 0 (+ 1 i))) ((>= i (length inbuf)))
      (set! (outbuf i) (* *vol* (inbuf i))))
    ; return a list of numchans and chans vectors
    (list 1 outbuf)))
    
(define (perform-stereo inL inR)
  (let* ((outL (make-vector (length inL) 0.0))
         (outR (make-vector (length inR) 0.0)))
    (do ((i 0 (+ 1 i))) ((>= i (length inbuf)))
      (set! (outL i) (* *vol* (inL i)))
      (set! (outR i) (* *vol* (inR i)))
      )
    ; return a list of numchans and chans vectors
    (list 2 outL outR)))

