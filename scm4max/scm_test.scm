(define mynum 5) 

;; not (args) as this is a regular scheme function, not a dispatch function
(define add
  (lambda args  
    (post "add" args)
    (apply + args)))
    

(listen 1 (lambda (args)
    (post "inlet 1 listener, args: " args)))

(post "scm_test.scm done reload...")
