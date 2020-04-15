;;(post "s4m_help_listeners.scm loading....")

;; Demo scheme-for-max code for the Listeners help tab

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlet 0 handling, messages eval'd as scheme code
;; max symbols will be evaluated as scheme variables, and will be errors if undefined

;; to handle int, float, or bang messages on inlet 0, we define functions named accordingly
(define (f-bang) 
    (post "f-bang executing, send out bangs")
    (out 0 'bang) (out 1 'bang))

(define (f-int arg) 
    (post "f-int executing, adding 100 to " arg)
    (out 0 (+ 100 arg)))

(define (f-float arg) 
    (post "f-float executing, dividing 1 by " arg)
    (out 0 (/ 1 arg)))

;; a list to inlet 0 that starts with a number will be internally (in max) processed
;; as the message "list ...args..."
;; but as 'list' is a scheme function, we will intercept those with functions called 'f-list'
(define (f-list args) 
    (post "f-list executing, args: " args)
    ;; sent the length of the list out outlet 0
    (out 0 (length args))
    ;; send out the values in a sequence of messages out outlet 1, in reverse
    ;; out-1 is a single symbol convenience function, they exists up to out-8
    (map out-1 (reverse args)) 
)

(define (my-sum arg-1 arg-2) 
    (out 0 (+ arg-1 arg-2))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlet 1+ handling, messages dispatched to registered listeners
;; listeners for int, float, bang, and list use keywords :int, :float, :bang, :list
;; messages won't do anything unless picked up by a registered listener function
;; listeners must be functions with one argument, which will be handed a list (possibly empty)
;; max symbols will become scheme symbols

;; a sample listener, signature is always one param that will be a list of args
(define (num-listener args) (post "num-listener: " (args 0)))

;; register the listener to be called on int messages to inlet 1 (note keyword :int) 
(listen 1 :int num-listener)
;; register the same function to be called on float messages to inlet 1 
(listen 1 :float num-listener)

;; defining a bang listener anonymously and register it on inlet 1
;; note that it uses the standard listener signature, but args will be the empty list
(listen 1 :bang (lambda (args) (post "bang listener!")))

;; listen for a list of integers or floats (or mix)
(define (list-listener args) (post "list listener, args: " args))
(listen 1 :list list-listener)

;; a function to take in two elements of a list and send them out the outlets swapped
;; Note that listener functions use one argument, and this argument always gets
;; the list of args bundled into a list. We don't have the listener to this bundling
;; for us, it's done already by the dispatcher so that all listeners can have the 
;; same signature and handle any number of args
(define (swap-out args) 
    (post "swap-outlets args: " args)
    (out 0 (args 1))
    (out 1 (args 0)))

;; register the above to listen on inlet 1, for messages starting with the keyword :swap
;; we could also register by quoting a symbol as the key 
;; keywords are good for avoiding confusion about whether symbols will be evaluated
;; as keywords always eval to themselves, so there's no ambiguity when looking at Max
(listen 1 :swap swap-out)
(listen 1 'swap swap-out)

;; Contrast thed def of swap-out with the version below, meant to be called as scheme code with 
;; an unknown number of args coming in. Bundling them is happening in this function def.
;; that means the below will work fine on inlet 0 with message: "swap-out-2 'foo 'bar"
;; it won't throw an error on "swap-out-2 'foo 'bar 'baz", it'll just ingore 'baz
(define swap-out-2 (lambda args 
    (post "swap-out-2 args: " args)
    (out 0 (args 1))
    (out 1 (args 0))))

;; And this version, which will not harmlessly ignore extra args, it needs 2!
(define (swap-out-3 arg-1 arg-2) 
    (post "swap-out-3 args: " arg-1 arg-2)
    (out 0 arg-1)
    (out 1 arg-0))
    
;; if you edit this file, you might want to uncomment the below as it
;; tells you that you got through loading ok.
(post "s4m_help_listeners.scm DONE")



