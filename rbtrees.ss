; rbtrees.ss - Red-Black trees
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

#|
    Title: Red-Black trees

    Example:
    (start code)
    (end code)

    library: (rbtrees)

|#
(library (rbtrees)
  (export
   rb-trees?
   rb-valid?
   rb-set!
   make-rb-trees
   )
  (import (rnrs))

(define-record-type rb-trees
  (fields
   (mutable root))
  (protocol
   (lambda (c)
     (lambda ()
       (c '())))))

(define-record-type node
  (fields
   (mutable left)
   (mutable right)
   (mutable parent)
   (mutable key)
   (mutable value)
   (mutable color)))

(define (rb-set! rb key value)
  (let loop ([x (rb-trees-root rb)]
             [y '()])
    (cond
     [(null? x)
      (let ([z (make-node '() '() y key value 'black)])
        (cond
         [(null? y)
          (rb-trees-root-set! rb z)]
         [(< key (node-key y))
          (node-left-set! y z)]
         [else
          (node-right-set! y z)]))]
     [else
      (if (< key (node-key x))
          (loop (node-left x) x)
          (loop (node-right x) x))])))

(define (rb-valid? rb)
  (assert (rb-trees? rb))
  (cond
   [(null? (rb-trees-root rb)) #t]
   [else
    (and (binary-search-tree? rb)
         (eq? 'black (node-color (rb-trees-root rb)))
         (all-leaves-black? (rb-trees-root rb)))]))

;; todo leaf?
(define (all-leaves-black? node)
  (cond
   [(null? node) #t]
   [(and (null? (node-left node))
         (null? (node-right node)))
    (eq? 'black (node-color node))]
   [else
    (and (all-leaves-black? (node-left node))
         (all-leaves-black? (node-right node)))]))

;; internal procedures
(define (binary-search-tree? rb)
  (define (rec node)
    (if (null? node)
        '()
        (append (rec (node-left node))
                (list (node-key node))
                (rec (node-right node)))))
  (let ([all-keys (rec (rb-trees-root rb))])
    (equal? (list-sort >= all-keys) all-keys)))
)
