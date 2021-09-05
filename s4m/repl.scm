(post :hello)

(delay 1000 (lambda()(post "Delay firing, in isr:" (isr?))))
(clock-ms 10
  (lambda()(post "listen callback, in isr:" (isr?))))
(cancel-clock-ms)


(clock-ticks 480
  (lambda(tick)(post "clock-ticks callback, tick:" tick " in isr:" (isr?))))
(cancel-clock-ticks)
