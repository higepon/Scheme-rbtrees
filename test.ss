(import (rnrs)
        (srfi :78)
        (rbtrees))

(check (rb-trees? (make-rb-trees)) => #t)

(check-report)
