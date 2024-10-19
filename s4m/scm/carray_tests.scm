; TODO: adapt these to proper tests


(make-carray :foo :string 4)

; int test
(begin
  (post "RUNNING int array tests")
  (make-carray :ai :int 4)
  (define iv (vector 1 2 3 4))
  (carrsv :ai 0 iv)
  (post (carr->v :ai))
  (post "int ref:" (carray-ref :ai 0))
  (post "setting :ai 0 to" (carray-set! :ai 0 99))
  (post "int ref 0 now:" (carray-ref :ai 0))
  (post "PASSED")
)
; float test
(begin
  (post "RUNNING float array tests")
  (make-carray :af :float 4)
  (define fv (vector 1.1 2.2 3 4 5 6))
  (carrsv :af 0 fv)
  (post (carr->v :af))
  (post "float ref:" (carray-ref :af 0))
  (post "setting :af 0 to" (carray-set! :af 0 1.11))
  (post "float ref 0 now:" (carray-ref :af 0))
  (post "PASSED")
)
; string test
(begin
  (post "RUNNING string array tests")
  (make-carray :as :string 4)
  (define sv (vector 1.1 2 "foo" '(1 2 3)))
  (carrsv :as 0 sv)
  (post (carr->v :as))
  (post "string ref:" (carray-ref :as 0) (carray-ref :as 2))
  (post "setting :af 0 to string" (carray-set! :as 0 "foobar"))
  (post "setting :af 1 to string from list" (carray-set! :as 1 '(1 2 3)))
  (post "string ref 0 now:" (carray-ref :as 0))
  (post "string ref 1 now:" (carray-ref :as 1))
  (post "making new array with same name")
  ;(make-array :as :string 8)
  (post "PASSED")
)
;char test
(begin
  (post "RUNNING char array tests")
  (make-carray :ac :char 4)
  (define cv (vector 1.1 2 "foo" '(1 2 3)))
  (carrsv :ac 0 cv)
  (post (carr->v :ac))
  (post "carray-ref char:" (carray-ref :ac 0) (carray-ref :ac 2))
  (post "setting :ac 0 to string" (carray-set! :ac 0 "foo"))
  (post "setting :ac 1 to string from list" (carray-set! :ac 1 '(1 2 3)))
  (post "setting :ac 2 to too-long string" (carray-set! :ac 2 "i am too long to fit in the space provided"))
  (post "char ref 0 now:" (carray-ref :ac 0))
  (post "char ref 1 now:" (carray-ref :ac 1))
  (post "char ref 2 now:" (carray-ref :ac 2))
  (post "PASSED")
)


