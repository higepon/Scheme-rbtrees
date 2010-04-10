(import (rnrs)
        (srfi :78)
        (rbtrees))

(let ([rb (make-rb-trees)])
  (check (rb-trees? rb) => #t)
  (check (check-rb rb) => #t)
  (rb-set! rb 1 'val1)
  (check (check-rb rb) => #t)
  (rb-set! rb 2 'val2)
  (check (check-rb rb) => #t))

(check-report)
