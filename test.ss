(import (rnrs)
        (srfi :78))

(check (rb-trees? (make-rb-trees)) => #t)

(check-report)
