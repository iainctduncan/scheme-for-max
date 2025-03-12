(post "spectral-centroid.scm loading")

(define (spectral-centroid frames)
  (post "spectral-centroid")

  (let ((sum 0.0001)
        (centroid 0.00001))

    (do ((i 0 (+ 1 i))) ((>= i frames))
      (let* ((sig-l (float-vector-ref in-l i)))
        (set! sum (+ sum sig-l))))      
    
    (do ((i 0 (+ 1 i))) ((>= i frames))
      (let* ((sig-l (float-vector-ref in-l i)))
        (set! centroid (+ centroid (* i (/ sig-l sum))))))

    (set! centroid (/ centroid frames))
    (post "sum:" sum)
    (post "centroid:" centroid)
    ))
     

(define (perform frames)
  (spectral-centroid frames))

(post "spectral-1.scm loaded")
