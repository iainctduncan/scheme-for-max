(post "s4m-score-demo.scm loading")

(load-from-max "helpers.scm")
(load-from-max "cli.scm")
(load-from-max "s4m-score.scm")

; a reset function that will zero out the transport and metro
; and reset the s4m interpreter
(define (s4m-reset)
  (post "s4m-reset")
  (send 'transport 0)
  (send 'locate 0)
  (send 'metro 0)
  (delay 100 (lambda() (send 's4m-reset 'bang))))

; function to play a note, just outputs a list
; out s4m's outlet 0 to be used with makenote
(define (n num vel dur)
  (out 0 (list num vel dur))
)

;***************************************************************************************************
; score-1 
; a score that plays two bars three times, using a private score variable to count down

(define score-1 (make-score (hash-table :bbt '(8 4 480) :res 2 :name "Score-1") `(
  ; make a variable called "repeats", local to the score instance
  :1-1  (var 'repeats 3)  

  ; two bars of notes
  :1-1  (n 60 90 120) 
  :1-2  (n 64 90 120)
  :1-3  (n 67 90 120)
  :1-4  (n 64 90 120)

  :2-1  (n 48 90 120) 
  :2-2  (n 52 90 120)
  :2-3  (n 55 90 120)
  :2-4  (n 52 90 120)
  
  ; decrement rep and repeat if not zero
  :3-1  (if (> (dec! repeats) 0) 
            (go :1-1))
  ; a final note if we don't jump
  ; all events in the outer parens get evaluated
  :3-1  ((n 60 90 480) (stop))          
  )))

(define (play-score-1)
  (post "cueing and starting score-1")
  (score-1 'cue 1)
  (score-1 'start)
  (score-3 'var-set 'repeats 4))

;***************************************************************************************************
; score-2 
; a score that controls the tempo by sending max messages to a line object

; function to send a tempo envelope, expects a Max line object
; with scripting name "tempo-line"
(define (tempo-env start ms end)
  (send 'tempo-line start)
  (send 'tempo-line end ms))

(define (play-score-2)
  (post "cueing and starting score-1")
  (send 'transport 'tempo 120)
  (score-2 'cue 1)
  (score-2 'start))

(define (stop-score-2)
  (post "stopping score-2 and transport")
  (score-2 'stop)
  (send 'metro 0) 
  (send 'transport 0)
  (send 'transport 'tempo 120))

(define score-2 (make-score (hash-table :bbt '(8 4 480) :res 2 :name "score-2") `(
  ; zero and start the transport, and start the synced metronome
  :1-1  ((send 'locate 0) (send 'transport 1) (send 'metro 'bang))
  :1-1  (n 60 90 120) 
  :1-2  (n 64 90 120)
  :1-3  (n 67 90 120)
  :1-4  (n 64 90 120)

  ; fire our tempo env function
  :2-1  (tempo-env 120 4000 60)

  :2-1  (n 48 90 120) 
  :2-2  (n 52 90 120)
  :2-3  (n 55 90 120)
  :2-4  (n 52 90 120)

  :3-1  (n 60 90 120) 
  :3-2  (n 64 90 120)
  :3-3  (n 67 90 120)
  :3-4  (n 64 90 120)

  :4-1  (n 48 90 120) 
  :4-2  (n 52 90 120)
  :4-3  (n 55 90 120)
  :4-4  (n 52 90 120)
  
  ; stop score, metro, and transport
  :5-1  (n 60 90 480) 
  :5-1  (stop-score-2)
)))


;***************************************************************************************************
; score-3 
; a score that used Lisp unquoting to programmatically inject material

(define (play-score-3)
  (post "cueing and starting score-3")
  (send 'transport 'tempo 120)
  (score-3 'start)
  (score-3 'var-set 'repeats 4)
  )

; defs and functions to create material we will use in the score
(define note-data-preset (list 72 90 120))

(define (random-note)
  (n (+ 60 (random 12)) 90 120))

(define (make-events)
  "return an association list to inject in score"
  (let ((note-num (+ 48 (random 12))))
    `(
      :2-1 (n ,note-num 90 120)
      :2-3 (n ,note-num 90 120) 
     )))

(define score-3 (make-score (hash-table :bbt '(8 4 480) :res 480 :name "Score-3") `(
  :1-1  (var 'repeats 4)  

  :1-1  (n 60 90 120)
  ; calls random-note at runtime, changes each pass
  :1-2  (random-note)

  ; splice in node-data, uses values at instantiation time
  :1-3  (n ,@note-data-preset)

  ; splice in return value from make events function, including times
  ; will not change on each pass as we are unquote splicing
  ,@(make-events)

  ; repeat reps times, else stop
  :3-1  (if (> (dec! repeats) 0) (go :1-1) (stop))
  :3-1  (n 60 90 480) 
)))




