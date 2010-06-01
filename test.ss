(import (rnrs)
        (srfi :27)
        (srfi :78)
        (rbtrees))

(let ([rb (make-rb-trees)])

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


)

(let ([rb (make-rb-trees)])
  (do ([i 0 (+ i 1)])
      ((= i 100))
    (rb-set! rb i i))
  (check (check-rb rb) => #t))

(let ([rb (make-rb-trees)])
  (let ([port (open-output-file "rb-trees.dot")])
    (do ([i 0 (+ i 1)]
         [j (random-integer 100000) (random-integer 100000)])
        ((= i 100))
      (rb-set! rb j j))
    (check (check-rb rb) => #t)
    (rb->dot rb port)
    (close-port port)))

(check-report)
