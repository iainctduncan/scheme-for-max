; s4m-live-example.scm - a drunken walk sequencer
; plays random pentatonic notes that are within a M3rd
(post "loading s4m-live-random-walk.scm??")

; two octaves of pentatonic and a base midi note number
(define pentatonic-notes '(0 2 4 7 9 12 14 16 19 21 24))
(define base-note 48)

; global state variables 
(define g-transpose 0)
(define g-last-note 12)
(define g-step-length 120)
(define g-running #f)
(define g-delay-handle #f)

; function to make a drunken walk choice given candidates and last note
(define (choose-note candidates last-note)
  ; filter candidates to those within a major third, but not the same note
  (let* ((filtered (filter 
                       (lambda(x)(and 
                          (not (= x last-note))
                          (<= (abs (- last-note x)) 4))) 
                       candidates)))
    ; return random element from the filtered list
    (filtered (random (length filtered)))))

; run-step is the function that runs on each step
; sends out a new generated note and then 
; reschedules itself if the engine is running
(define (run-step)
  (let* ((note-choice (choose-note pentatonic-notes g-last-note))
         ; adjust by base note and transpose
         (note-num (+ base-note g-transpose note-choice))
         ; random velocity from 60 to 120
         (vel (+ 60 (random 60)))
         ; duration is 90% of step length
         (dur (* g-step-length 0.75)))
    ; update our last note state for the next choice
    (set! g-last-note note-choice)      
    ; send out our note, out* spreads elements over available outlets
    (out* (list note-num vel dur))
    ; schedule the play-note function after g-step-length ticks
    ; we use delay-tq for ticks, quantized
    (if g-running
      ; save the delay handle so we can cancel an oustanding delay
      (set! g-delay-handle 
        (delay-tq g-step-length g-step-length run-step)))))

(define (start)
  (post "(start)")
  (set! g-running #t)
  (run-step))

(define (stop)
  (post "(stop)")
  (set! g-running #f)
  ; cancel any outstanding delay call to stop immediately
  (cancel-delay g-delay-handle))

; tranport listeners, only called when transport changes
; these are hooked up to plugsync~
(define (set-transport state)
  (if (= state 1) (start) (stop)))


