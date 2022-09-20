(post :hello)

(s4m-send-live-path 'live_set 'tracks 0 'mixer_device 'volume) 

(send 's4m-live-path '(path live_set tracks 0 mixer_device volume)) 

(send 's4m-live-path 'path 'live_set 'tracks 0 'mixer_device 'volume) 

(post 'foo)

; the below works, sends a path message
)
