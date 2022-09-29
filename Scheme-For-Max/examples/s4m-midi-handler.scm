; midi-handler.scm
; an template file and patcher for you to build on for making midi processing tools with s4m

(post "midi-handler.scm loading")

; function that runs on the 'stop' message
(define (start)
  (post "(start)")
  ; the transport toggle has the scripting name 'transport-toggle',
  ; so we can start the global transport with the below
  (send 'transport-toggle 1)
)

; function that runs on the 'stop' message
(define (stop)
  (post "(start)")
  (send 'transport-toggle 0)
)

; pick up raw midievent messages
; you'll need to look up the midi spec to decode
; as b1 interleaves channel and type!
(define (midievent b1 b2 b3)
  (post "midievent" b1 b2 b3)
)

; listener function called from the transport object listener
; state will be 1 on play and 0 on stop
(define (transport state)
  (cond
    ((= state 1)
      (post "transport started"))
    (else
      (post "transport stopped"))))



  

