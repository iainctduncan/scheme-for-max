(define (run-table-examples) 

  ;; run in a with-let with the global environment to allow these 
  ;; definitions to be run in the global scope. This allows you to 
  ;; use these definitions in the repl even though we defined them in a function
  (with-let (rootlet) 
    (post "(run-table-examples)")

    (post "(table? 't1) " (table? 't1))

    ;; get table length
    (post "(table-length 't1) " (table-length 't1))

    ;; read from a table
    (post "(table-ref 't1 2) " (table-ref 't1 2))

    ;; write to a table
    (post "(table-set! 't1 3 99) " (table-set! 't1 3 99))

    ;; define a vector and write to a table at index 3
    (define vec1 (vector 32 64 96 127))
    (post "(table-set-from-vector! 't1 3 vec1) " (table-set-from-vector! 't1 3 vec1))

    ;; define another vector and load from a table
    (define vec2 (vector 32 64 96 127))
    (post "(table-set-from-vector! 't1 3 vec2) " (table-set-from-vector! 't1 3 vec2))
    (post "vector now: " vec2)

  )) ;; end of with-let and function

(define (run-buffer-examples) 

  ;; run in a with-let with the global environment to allow these 
  ;; definitions to be run in the global scope. This allows you to 
  ;; use these definitions in the repl even though we defined them in a function
  (with-let (rootlet) 
    (post "(run-buffer-examples)")

    (post "(buffer? 'b1) " (buffer? 'b1))

    ;; get number of samples in buffer
    (post "(buffer-size 'b1) " (buffer-size 'b1))

    ;; read from a buffer
    (post "(buffer-ref 'b1 2) " (buffer-ref 'b1 2))

    ;; write to a buffer
    (post "(buffer-set! 'b1 3 99) " (buffer-set! 'b1 3 99))

    ; make a vector 
    (define vec (vector 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1.0))
    
    ;copy vector into buffer in one operation
    (buffer-set-from-vector! 'b1 0 vec)
    
    ;copy partial contents           chan   index           index   count
    (buffer-set-from-vector! 'b1 0      2       vec     4       2)
    
    ; make a vector from buffer contents
    (define v2 (buffer->vector 'b1))
    (post v2)
    
    ; make a vector from only 4 points starting at index 2
    ;                       chan start  count
    (buffer->vector 'b1 0    2      2)

    ;; define a vector and write to a buffer at index 3
    (define vec1 (vector 32 64 96 127))
    (post "(buffer-set-from-vector! 'b1 3 vec1) " (buffer-set-from-vector! 'b1 3 vec1))

    ;; define another vector and load from a buffer
    (define vec2 (vector 32 64 96 127))
    (post "(buffer-set-from-vector! 'b1 3 vec2) " (buffer-set-from-vector! 'b1 3 vec2))
    (post "vector now: " vec2)
  
  )) ;; end of with-let and function

