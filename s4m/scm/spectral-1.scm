(post "spectral-1.scm loading...")

(define *vol* 0.0)
(define *bin* 0)

; very weird that I have to print the inL to make it work first
; some kind of timing error
(define (perform inL inR)
  (post "perform")
  (post "  - inL" inL)
  (post "  - inR" inR)
  (let* ((outL (make-vector (length inL) 0.0))
         (outR (make-vector (length inR) 0.0)))

    (do ((i 0 (+ 1 i))) ((>= i (length inL)))
      (let ((bin-vol (if (< i *bin*) *vol* 1.0)))
        (set! (outL i) (* bin-vol (inL i)))
        (set! (outR i) (* bin-vol (inR i)))
        ))
    ; return a list of numchans and chans vectors
    (list outL outR)))


(post "spectral-1.scm loaded")
