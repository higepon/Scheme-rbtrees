(import (rnrs)
        (srfi :27)
        (srfi :78)
        (mosh)
        (rbtrees))

(let ([rb (make-rb-trees = <)])
  (do ([i 0 (+ i 1)])
      ((= i 100))
    (rb-set! rb i i))
  (check (check-rb rb) => #t))

(let ([rb (make-rb-trees = <)])

  (check (rb-get rb 1 'not-found) => 'not-found)

  (check (check-rb rb) => #t)
  (rb-set! rb 1 'val1)
  (check (rb-get rb 1) => 'val1)
  (check (check-rb rb) => #t)

  (rb-set! rb 2 'val2)
  (check (rb-get rb 1) => 'val1)
  (check (rb-get rb 2) => 'val2)

  (rb-set! rb 5 'val5)
  (check (rb-get rb 1) => 'val1)
  (check (rb-get rb 2) => 'val2)
  (check (rb-get rb 5) => 'val5)
  (check (check-rb rb) => #t)

  (rb-set! rb 3 'val3)
  (check (rb-get rb 1) => 'val1)
  (check (rb-get rb 2) => 'val2)
  (check (rb-get rb 3) => 'val3)
  (check (rb-get rb 5) => 'val5)
  (check (check-rb rb) => #t)

  (rb-set! rb 4 'val4)
  (check (rb-get rb 1) => 'val1)
  (check (rb-get rb 2) => 'val2)
  (check (rb-get rb 3) => 'val3)
  (check (rb-get rb 4) => 'val4)
  (check (rb-get rb 5) => 'val5)
  (check (check-rb rb) => #t)

  (rb-set! rb 0 'val0)
  (check (rb-get rb 0) => 'val0)
  (check (rb-get rb 1) => 'val1)
  (check (rb-get rb 2) => 'val2)
  (check (rb-get rb 3) => 'val3)
  (check (rb-get rb 4) => 'val4)
  (check (rb-get rb 5) => 'val5)
  (check (check-rb rb) => #t)

  (rb-delete! rb 1)
  (check (rb-get rb 1 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

  (rb-delete! rb 5)
  (check (rb-get rb 5 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

  (rb-delete! rb 3)
  (check (rb-get rb 3 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

  (rb-delete! rb 0)
  (check (rb-get rb 0 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

  (rb-delete! rb 2)
  (check (rb-get rb 2 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

  (rb-delete! rb 4)
  (check (rb-get rb 4 'not-found) => 'not-found)
  (check (check-rb rb) => #t)

)




(let ([rb (make-rb-trees string=? string<?)])

  (check (rb-get rb "abc" 'not-found) => 'not-found)

  (rb-set! rb "abc" 'val1)
  (check (rb-get rb "abc") => 'val1)
  (check (check-rb rb) => #t)

  (rb-set! rb "def" 'val2)
  (check (rb-get rb "def") => 'val2)
  (check (check-rb rb) => #t)

  (rb-delete! rb "abc")
  (check (rb-get rb "abc" 'not-found) => 'not-found)
  )

#;(let ([rb (make-rb-trees = <)])
  (let ([port (open-output-file "rb-trees.dot")])
    (do ([i 0 (+ i 1)]
         [j (random-integer 1000000) (random-integer 1000000)])
        ((= i 100000))
      (rb-set! rb j j))
    (check (check-rb rb) => #t)
    (time (rb-get rb 1234 1))
    (close-port port)))

;; TODO
;; num-entries
;; unify name-prefix
;; keys

(check-report)
