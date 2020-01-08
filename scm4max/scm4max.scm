;; the scm4max scheme code to build the API
(define debug-args (list))

(define s4m-listeners (make-hash-table))

(define s4m-add-listener
  (lambda (sym fun) 
    (post (string-append "add-listener " (symbol->string sym) ))
    (set! (s4m-listeners sym) fun)))

(define handle
  (lambda (args)
    (post (string-append "handle, arg count: " (number->string (length args))))))    

(define adder
  (lambda (args)
    ;;(set! debug-args args)
    (post (string-append "adder arg count: " (number->string (length args))))
    (apply + args)))

(define foo "bar")

(define s4m-dispatch
  (lambda args
    (post (string-append "s4m-dispatch arg count: " (number->string (length args))))
    ;;(set! debug-args args)
    ((s4m-listeners (car args)) (cdr args))))

;; register our listeners
(s4m-add-listener 'handle handle)
(s4m-add-listener 'adder adder)

(post "bootstrap complete")

