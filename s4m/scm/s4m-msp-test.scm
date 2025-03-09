(post "s4m-msp-test.scm loaded")

;(define (perform frame)
;  (if (= (modulo frame 12) 0)
;    (post "s4m~ (perform), frame:" frame))
;)

;(do ((i 0 (+ 1 i))) ((>= i (length buf)))
;   (set! (buf i) (* 0.1 (buf i))))


(define *vol* 1.0)

(define (perform inbuf) 
  (post "(perform)")
  (let ((outbuf (make-vector (length inbuf) 0.0)))
    (do ((i 0 (+ 1 i))) ((>= i (length inbuf)))

      (set! (outbuf i) (* *vol* (inbuf i))))
    
    outbuf))

(post "perform function" perform)
