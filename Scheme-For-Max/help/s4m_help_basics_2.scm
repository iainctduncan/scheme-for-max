;; s4m_help_basics_2.scm - second source file for the "basics" help tab
(post "s4m_help_basics_2.scm loading....")
;; foobars

;; overwrite the 
(define my-file-2-var "my-file-2-var")

;; a new var, not defined in the first source file, and pointing to a keyword
(define my-var-2 )

;; a post at the end will let you know if the file finished loading without errors
(post "s4m_help_basics_2.scm done")
