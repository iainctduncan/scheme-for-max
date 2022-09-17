;; Source file for the basics tab of the scheme for max help
;; You'll want to use an editor with scheme/lisp syntax highlighting to help
;; balance parantheses.

;; Post to the max console
(post "Hello world, from S7 scheme!")
(post "message from scm_help_basic.scm")

;; Define some variables. 
;; ints and floats will keep their type when sent to max
(define my-int 99)
(define my-float 1.23)
(define my-list (list 1 2 3))

;; post can take all kinds of things (note: no commas)
(post "Some variables: " my-int my-float my-list)

;; Define a function that adds 1 to a number, 
;; posts the number to the console, and sends it out outlet 0
(define (my-adder num) 
    (post "my-adder executing, num is " num)
    (out 0 (+ 1 num)))

;; A function that does something with one of our variables
(define (inc-my-int) 
    (post "incrementing my-int by 1")
    (set! my-int (+ 1 my-int)))

;; sometimes it's helpful to post at the bottom of files to let you know 
;; loading is complete
(post "s4m_help_basics.scm done loading....")


