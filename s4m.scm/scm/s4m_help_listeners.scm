(post "s4m_help_listeners.scm loading....")

;; Demo scheme-for-max code for the Listeners help tab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlet 0 handling, messages eval'd as scheme code
;; max symbols will be evaluated as scheme variables, and will be errors if undefined

;; to handle int, float, or bang messages on inlet 0, we define functions named accordingly
(define (f-bang) (post "f-bang executing"))
(define (f-int arg) (post "f-int executing, arg:" arg))
(define (f-float arg) (post "(f-float executing, arg:" arg))

;; a list to inlet 0 that starts with a number will be internally (in max) processed
;; as the message "list ...args..."
;; but as 'list' is a scheme function, we will intercept those with functions called 'flist'
;; note that we are using the lambda args form to bundle variable number of items into args
(define f-list (lambda args (post "f-list executing, args: " args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlet 1+ handling, messages dispatched to registered listeners
;; messages won't do anything unless picked up by a registered listener function
;; listeners must be functions with one argument, which will be handed a list (possibly empty)
;; max symbols will become scheme symbols

;; a sample listener, signature is always one param that will be a list of args
(define (num-listener args) (post "num-listener: " (args 0)))

;; register the listener to be called on int messages to inlet 1 
(listen 1 :int num-listener)
;; register the same function to be called on float messages to inlet 1 
(listen 1 :float num-listener)

;; defining a bang listener anonymously and register it on inlet 1
;; note that it uses the standard listener signature, but args will be the empty list
(listen 1 :bang (lambda (args) (post "bang listener!")))

;; listen for a list of integers or floats (or mix)
(define (list-listener args) (post "list listener, args: " args))
(listen 1 :list list-listener)

;; a listener defined for a symbolic message
;; in this case, we register on the symbol that will be the first token of the 
;; max message. (i.e. "my-func 1 2 3")
(define (foobar-listener args) (post "foobar-listener, args: " args))
;; we could register by quoting a symbol as the key, or using a keyword
;; keywords are recommended to avoid confusion about whether symbols will be evaluated
;; as keywords always eval to themselves, so there's no possible ambiguity
(listen 1 :foobar foobar-listener)
(listen 1 'foobar foobar-listener)

(post "s4m_help_listeners.scm DONE")



