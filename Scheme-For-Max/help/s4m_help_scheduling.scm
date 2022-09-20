;; Examples of scheduling and clocks.
;; See documentation for more exmaples
;; https://github.com/iainctduncan/scheme-for-max-docs

;; function to play a click
(define (play-click)
  (out 0 :click))

; function to prevent barage of messages from hitting the help tab at once
(define (schedule-examples) 
 
  ; use with-let rootlet to make inner defs visible to global scoe 
  (with-let (rootlet)
    
    (define (play-note dur note vel)
      (out 0 (list dur note vel))) 

    (play-note 44 90 90)
    
    ; make a zero argument function to use with delay
    (define play-44 (lambda () (play-note 44 90 90)))
    (play-44)
    
    ; delay by 1 second
    (delay 1000 play-44)
    
    ; convenience wrappers
    (delay-eval 500 '(play-note 44 90 90))
    
    (delay-eval 500 play-note 44 90 90)
    
    ; get max current time (does not reset with transport rewind)
    (time)
    
    ; delay by 5 seconds but cancel it first
    (define handle (delay 3000 play-44))
    (cancel-delay handle)
    
    ; delay by 1/4
    (delay-t '4n play-44)
    (delay-t 480 play-44)
    
    ; delay by 1/2 but quantize to the bar
    ; note: this uses transport times, but will play even if transport is stopped
    (delay-tq '2n '1n play-44)
    
    ; repeating clocks
    (clock-ms 500 (lambda () (play-note 44 90 90)))
    (cancel-clock-ms)
    
    (clock-ms-t 500 (lambda () (play-note 44 90 90)))
    (cancel-clock-ms-t)
    
    (clock-ticks 480 (lambda (ticks) (play-note 44 90 90)))
    (cancel-clock-ticks)
    
    ;; you might want to make synchronization helpers 
    (define (start)
      (transport-seek 0)
      (clock-ticks 480 (lambda (ticks) (play-note 44 90 90)))
      (transport-state-set! #t))

    (define (stop)
      (transport-state-set! #f)
      (send 'metro-start 0)
      (cancel-clock-ticks))
    
    ; an example of self-scheduling with delay
    (define handle-1 #f)
    
    (define (play-recursive)
      (play-44)
      (set! handle-1 (delay-tq 480 480 play-recursive)))
    
    (delay-tq 480 480 play-recursive)
    
    (cancel-delay handle-1) 

  )); end with-let and function

