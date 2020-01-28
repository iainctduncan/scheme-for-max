(define mynum 5) 

;; not (args) as this is a regular scheme function, not a dispatch function
(define add
  (lambda args  
    (post "add" args)
    (apply + args)))
    
;; args from max come in as a list, so deref as (margs X)
(s4m-listen 'add
  (lambda m-args 
    (post "m-args" m-args)
    ;;(out-int (add (margs 0) (margs 1)))))
    (out-int (add margs))))

(s4m-listen 'sub
  (lambda (args) 
    (post "sub running")
    (out-int
      (- (args 0) (args 1)))))

;; register our listeners
(s4m-listen 'add add)

(post "scm_test.scm done reload...")
