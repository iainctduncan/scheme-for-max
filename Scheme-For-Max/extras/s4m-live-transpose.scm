; s4m-live-transpose.scm - a simple midi note transposer
; transposes note on/off messages, filters out any others
(post "loading s4m-live-transpose.scm")

; global variable of amount to transpose
(define g-transpose 0)

; function to change the tranpose value
(define (set-transpose value)
  (post "setting transpose to" value)
  (set! g-transpose value))

; function that receives note-on and note-off and transposes them
(define (note-in chan note-num vel)
  (post "note-in:" (list chan note-num vel))
  (let ((new-note-num (+ note-num g-transpose)))
    ; send channel and pitch/vel separately to match
    ; what midiformat object expects
    (out 1 chan)
    (out 0 (list new-note-num vel))))


