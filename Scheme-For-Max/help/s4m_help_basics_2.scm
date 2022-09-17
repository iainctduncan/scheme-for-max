;; s4m_help_basics_error - second source file for the "basics" help tab, with an error
(post "s4m_help_basics_2.scm loading....")

;; re-defining the inc-my-int function
(define (inc-my-int) 
    (post "incrementing my-int by 2")
    (set! my-int (+ 2 my-int)))

;; a post at the end will let you know if the file finished loading without errors
(post "s4m_help_basics_2.scm done")
