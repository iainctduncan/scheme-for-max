(post :hello)

(define v (vector 1 2 3))

(for-each
  (lambda(x)(post x))
  v)

(do ((i 0 (+ 1 i))) ((>= i 4))
  (post i))
