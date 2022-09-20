(define (run-dict-examples) 

  ;; run in a with-let with the global environment to allow these 
  ;; definitions to be run in the global scope. This allows you to 
  ;; use these definitions in the repl even though we defined them in a function
  (with-let (rootlet) 
    (post "(run-dict-examples)")

    ; read a simple value
    (post "(dict-ref 'd1 'a) " (dict-ref 'd1 'a))
    
    ; reading a nested dict returns a hash-table
    (post "(dict-ref 'd1 'b) " (dict-ref 'd1 'b))
 
    ; get a value from the hash-table returned using long or applicative syntax
    (hash-table-ref (dict-ref 'd1 'b) 'ba)
    ((dict-ref 'd1 'b) 'ba)
    
    ; get an array from a dict, becomes a vector
    ; we are using a keyword key here
    (post "(dict-ref 'd1 :c)" (dict-ref 'd1 :c))
    ((dict-ref 'd1 :c) 1)
    
    ; get a compound value from a dict
    (post "(dict-ref 'd1 :d)" (dict-ref 'd1 :d))
    
    ; set a value
    (post "(dict-set! 'd1 'a 99)" (dict-set! 'd1 'a 99))
    
    ; get a value, recursively
    (post "(dict-ref 'd1 (list :d :da :daa 2))" (dict-ref 'd1 (list :d :da :daa 2)))

    (post "see s4_help_dicts.scm file for more examples")
    
    ; bad list throws error
    ; (dict-ref 'd1 '(:d :da :dz 2))
    
    ; set using a list 
    (dict-set! 'd1 '(:d :dc 2 :e) 111)
    ; now get the new value
    (dict-ref 'd1 '(:d :dc 2 :e))
    
    ; dict replace will make the missing intermediate hierarchy
    (dict-replace! 'd1 '(:foo :bar) 999)
    (dict-ref 'd1 :foo)

  )) ;; end with-let and function
