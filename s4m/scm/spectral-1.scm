(post "spectral-1.scm loading")

(define vol 0.0)
(define bin 4)

; 1 bin
(define (performx frames)
  ;(post "spectral perform - frames:" frames)

  (do ((i 0 (+ 1 i))) ((>= i frames))
    (let ((bin-vol (if (= i bin) vol 0.0))
          (sig-l (float-vector-ref in-l i))
          (sig-r (float-vector-ref in-r i)))
      ;(if (or (= i 19) (= i 20))
      ;  (post "setting i: " i "to bin-vol:" bin-vol))

      ;(float-vector-set! out-l i (* sig-l bin-vol))
      ;(float-vector-set! out-r i (* sig-r bin-vol))
      (float-vector-set! out-l i (* sig-l bin-vol))
      ;(float-vector-set! out-r i sig-r)
      (float-vector-set! out-r i 0.0)
      
      )))


(define (random-phase frames)
  ;(post "random-phase")

  (do ((i 0 (+ 1 i))) ((>= i frames))
    (let ((sig-l (float-vector-ref in-l i))
          (sig-r (float-vector-ref in-r i)))

      (float-vector-set! out-l i sig-l)
      ;(float-vector-set! out-r i sig-r)
      ;(float-vector-set! out-r i (random (* 2 pi)))
      (float-vector-set! out-r i 0.0)
      )))


(define (perform frames)
  (random-phase frames))

(post "spectral-1.scm loaded?")
