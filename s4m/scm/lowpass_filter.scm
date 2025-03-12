(post "lowpass_filter.scm loading")


(define prev-l 0.0)
(define prev-r 0.0)


(define (perform frames)
  (post "lowpass filter perform - frames:" frames)

  (do ((i 0 (+ 1 i))) ((>= i frames))
    (let* ((sig-l (float-vector-ref in-l i))
          (sig-r (float-vector-ref in-r i))
          (filt-l (/ (+ sig-l prev-l) 2))
          (filt-r (/ (+ sig-r prev-r) 2)))
      (float-vector-set! out-l i filt-l)
      (float-vector-set! out-r i filt-r)
      (set! prev-l filt-l)
      (set! prev-r filt-r)
      )))



