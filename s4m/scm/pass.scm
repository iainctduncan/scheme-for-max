(post "pass.scm loading")


(define (perform frames)
  ;(post "pass perform - frames:" frames)

  (do ((i 0 (+ 1 i))) ((>= i frames))
    (let ((sig-l (float-vector-ref in-l i))
          (sig-r (float-vector-ref in-r i)))

      (float-vector-set! out-l i sig-l)
      (float-vector-set! out-r i sig-r)
      )))



