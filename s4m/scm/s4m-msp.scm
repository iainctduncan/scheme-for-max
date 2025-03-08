;; the scm4max scheme code to build the API
(max-post "Bootstrapping s4m-msp.scm. I come from scheme!")

;; stuff.scm contains the scheme level s7 helper definitions and must be loaded for various
;;(define s4m-done-bootstrap #t)

;*******************************************************************************
;; text output functions - same as in s4m (refactor later)
;; helper for building string reps for posting to console
(define (str-repr . args) 
 (let ((repr (lambda x (string-append (object->string x) " "))))
   (apply string-append (map repr args))))

;; convert to a string for our post function
(define (stringify item)
  (cond
    ((keyword? item) (symbol->string item))
    ((symbol? item) (string-append "'" (symbol->string item)))
    ((number? item) (number->string item))
    ((string? item) item)
    ((procedure? item) "<procedure>")
    ((and (boolean? item) (eq? item #t)) "#true")
    ((and (boolean? item) (eq? item #f)) "#false")
    (else (object->string item)))) 

;; post arbitrary args to the max console
;; allows calling like (post "my thing" thing "other thing" other-thing)
(define (post . args)
  (letrec (
    (log-string (lambda (lat)
                  (cond 
                    ((null? lat) "")
                    (else (string-append (stringify (car lat)) " " (log-string (cdr lat))))))))
    (max-post (log-string args))
))

(post "s4m-msp.scm init complete")
