(run-expr (post %1 %2 %3) (list 11 22 33))

(s4m-run-expr "(+ %1 %2 %3)")
(s4m-run-expr "(+ 9 %1 %2 %3)")

(s4m-run-expr "(post (+ 1 (+ %1 %2 %3)))")

(for-each 
  (lambda(t)(post "token" t "is sequence:" (sequence? t)))
  (string->sexp "(+ 1 (+ %1 %2 %3))"))

(list? (s4m-expr-inputs 2))
(s4m-expr-inputs 1)
