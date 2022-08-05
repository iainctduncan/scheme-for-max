; TODO: adapt these to proper tests

; int test
(begin
  (post "RUNNING int array tests")
  (make-array :ai :int 4)
  (define iv (vector 1 2 3 4))
  (arrsv :ai 0 iv)
  (post (a->v :ai))
  (post "int ref:" (array-ref :ai 0))
  (post "setting :ai 0 to" (array-set! :ai 0 99))
  (post "int ref 0 now:" (array-ref :ai 0))
  (post "PASSED")
)
; float test
(begin
  (post "RUNNING float array tests")
  (make-array :af :float 4)
  (define fv (vector 1.1 2.2 3 4 5 6))
  (arrsv :af 0 fv)
  (post (a->v :af))
  (post "float ref:" (array-ref :af 0))
  (post "setting :af 0 to" (array-set! :af 0 1.11))
  (post "float ref 0 now:" (array-ref :af 0))
  (post "PASSED")
)
; string test
(begin
  (post "RUNNING string array tests")
  (make-array :as :string 4)
  (define sv (vector 1.1 2 "foo" '(1 2 3)))
  (arrsv :as 0 sv)
  (post (a->v :as))
  (post "string ref:" (array-ref :as 0) (array-ref :as 2))
  (post "setting :af 0 to string" (array-set! :as 0 "foobar"))
  (post "setting :af 1 to string from list" (array-set! :as 1 '(1 2 3)))
  (post "string ref 0 now:" (array-ref :as 0))
  (post "string ref 1 now:" (array-ref :as 1))
  (post "PASSED")
)
;char test
(begin
  (post "RUNNING char array tests")
  (make-array :ac :char 4)
  (define cv (vector 1.1 2 "foo" '(1 2 3)))
  (arrsv :ac 0 cv)
  (post (a->v :ac))
  (post "array-ref char:" (array-ref :ac 0) (array-ref :ac 2))
  (post "setting :ac 0 to string" (array-set! :ac 0 "foobar"))
  (post "setting :ac 1 to string from list" (array-set! :ac 1 '(1 2 3)))
  (post "setting :ac 2 to too-long string" (array-set! :ac 2 "i am too long to fit in the space provided"))
  (post "char ref 0 now:" (array-ref :ac 0))
  (post "char ref 1 now:" (array-ref :ac 1))
  (post "char ref 2 now:" (array-ref :ac 2))
  (post "PASSED")
)


