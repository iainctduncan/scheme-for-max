;; Source file for the basics tab of the scheme for max help
;; You'll want to use an editor with scheme/lisp syntax highlighting to help
;; balance parantheses.

;; Post to the max console
(post "Hello world, from S7 scheme!")

;; Define some variables. 
;; ints and floats will keep their type when sent to max
(define my-int 99)
(define my-float 1.23)
(define my-list (list 1 2 3))

;; post can take all kinds of things (note: no commas)
(post "some vars: " my-int my-float my-list)

;; Define a function that adds 1 to a number, 
;; posts the number to the console, and sends it out outlet 0
(define (my-adder num) 
    (post "my-fun-1 executing, num is " num)
    (out 0 (+ 1 num)))

;; It's a good idea to have a message at the bottom to let you 
;; know your source file loaded ok without crashing, at least
;; until error messages get improved! 
(post "s4m_help_basics.scm done loading....")
