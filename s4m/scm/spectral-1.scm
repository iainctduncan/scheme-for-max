(post "spectral-1.scm loading")

(define vol 1.0)
(define bin 512)

(define (perform frames)
  (post "spectral perform - frames:" frames)

  (do ((i 0 (+ 1 i))) ((>= i frames))
    (let ((bin-vol (if (<= i bin) vol 1.0))
          (sig-l (float-vector-ref in-l i))
          (sig-r (float-vector-ref in-r i)))
      ;(post "setting i: " i)

      (float-vector-set! out-l i (* sig-l bin-vol))
      (float-vector-set! out-r i (* sig-r bin-vol))
      
      )))




(post "spectral-1.scm loaded?")
