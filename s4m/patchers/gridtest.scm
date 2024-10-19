(post "gridtest.scm")

; make an s4m-array
(post "making carray")
(make-carray :array :string 128)

(define v (make-vector 128))
(dotimes (i 128) 
  (set! (v i) (format #f "~d" i))
)

(carray-set-from-vector! :array 0 v)  


(post "done")
