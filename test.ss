(import (rnrs)
        (srfi :78)
        (rbtrees))

(let ([rb (make-rb-trees)])
  (check (rb-trees? rb) => #t))

(check-report)
