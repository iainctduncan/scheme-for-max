(post "loaded...")

(define-macro (foo x) 
  `(+ 1 ,x))


(define y 5)
;(post "(foo 5) :" (foo 5))
;(post "(foo y) :" (foo y))
;(post (macroexpand (foo y)))

; what we want

;(1 ~> + 2)

;(:> 1 :> + 2)

; should become
;(+ (eval 1) 2)

; the below works for (~> + 1 2 3)
; produces (eval (list + 1 2 3))
;(define-macro (~> . args)
;  `(eval (list ,@args)))
;
;(post (macroexpand (~> + 1 2 3)))
;(post (~> + 1 2 3))

;- we need to unspool args until we get to the macro symbol
; consume from args until the ~> is reached

;(post (macroexpand (~> + 1 ~> + 2)))

;- does this even need to be a macro?
;(~> 1 ~> + 2 3 ~> + 4 5)
;(+ 3 (+ 2 (eval 1))) 

; consumes the list until there is a token
(define (~> . args)
  (cond ( (null? args) 
            '())
        ( (not (eq? '~> (car args)))
            (cons (car args) (~> (cdr args)))
        ))

(post (~> + 1 2 3))
