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

(define (node-fold proc init node)
  (cond
   [(not (null? (node-left node)))
    (let ([x (proc (node-fold proc init (node-left node)) node)])
      (if (not (null? (node-right node)))
          (proc x (node-right node))
          x))]
   [else
    (let ([x (proc init node)])
      (if (not (null? (node-right node)))
          (proc x (node-right node))
          x))]))


(define (rb-valid? rb)
  (assert (rb-trees? rb))
  (cond
   [(null? (rb-trees-root rb)) #t]
   [else
    (display (black-hight-same? (rb-trees-root rb)))
    (and (binary-search-tree? rb)
         (black? (rb-trees-root rb))
         (all-leaves-black? (rb-trees-root rb))
         (red-has-two-black? (rb-trees-root rb))
         (node-fold (lambda (accum node) (and accum (black-hight-same? node))) #t (rb-trees-root rb)))]))

(define (leaf? node)
  (and (null? (node-left node))
       (null? (node-right node))))

(define (black? node)
  (eq? 'black (node-color node)))

(define (red? node)
  (eq? 'red (node-color node)))


(define (all-leaves-black? node)
  (cond
   [(null? node) #t]
   [(leaf? node)
    (black? node)]
   [else
    (and (all-leaves-black? (node-left node))
         (all-leaves-black? (node-right node)))]))

(define (red-has-two-black? node)
  (cond
   [(null? node) #t]
   [(red? node)
    (and (black? (node-left node))
         (black? (node-right node))
         (red-has-two-black? (node-left node))
         (red-has-two-black? (node-right node)))]
   [else
    (and (red-has-two-black? (node-left node))
         (red-has-two-black? (node-right node)))]))

(define (black-hight-same? node)
  (define height* '())
  (define (add-height! h)
    (set! height* (cons h height*)))
  (define (rec h node)
    (cond
     [(null? node) '()]
     [else
      (let ([h (if (black? node) (+ h 1) h)])
       (and (not (null? (node-left node)))
            (rec h (node-left node)))
       (and (not (null? (node-right node)))
            (rec h (node-right node)))
       (add-height! h))]))
  (rec 0 node)
  (let ([height (car height*)])
    (for-all (lambda (x) (= height x)) height*)))



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
