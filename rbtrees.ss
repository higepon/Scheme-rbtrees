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
   check-rb
   rb-set!
   make-rb-trees
   rb->dot
   )
  (import (rnrs)
          (srfi :48))

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
          (node-color-set! z 'black)
          (rb-trees-root-set! rb z)]
         [(< key (node-key y))
          (node-color-set! z 'red)
          (node-left-set! y z)]
         [else
          (node-color-set! z 'red)
          (node-right-set! y z)]))]
     [else
      (if (< key (node-key x))
          (loop (node-left x) x)
          (loop (node-right x) x))])))

(define (node-fold init proc node)
  (cond
   [(null? node)
    init]
   [else
    (let ([accum (proc init node)])
      (node-fold (node-fold accum proc (node-left node)) proc (node-right node)))]))


(define (check-rb rb)
  (define (raise-error reason)
    (error 'check-rb reason))
  (assert (rb-trees? rb))
  (cond
   [(null? (rb-trees-root rb)) #t]
   [else
    (and (or (binary-search-tree? rb)
             (raise-error "not binary-search-tree"))
         (or (black? (rb-trees-root rb))
             (raise-error "root is not black"))
;;          (or (all-leaves-black? (rb-trees-root rb))
;;              (raise-error "all-leaves are not black"))
         (or (red-has-two-black? (rb-trees-root rb))
             (raise-error "red should have black childlen"))
         (or (node-fold #t (lambda (accum node) (and accum (black-hight-same? node))) (rb-trees-root rb))
             (raise-error "black height should be same")))]))

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
    (and (or (null? (node-left node)) (black? (node-left node)))
         (or (null? (node-right node)) (black? (node-right node)))
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

(define (rb->dot rb . port)
  (define (print-node-color node port)
    (if (black? node)
        (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" (node-key node))
        (format port "    ~s [style = filled, color = \"#336666\", fillcolor = \"#CC9999\"];\n" (node-key node))))
  (let ([port (if (pair? port) (car port) (current-output-port))])
    (format port "digraph rbtrees {\n")
    (node-fold '() (lambda (accum node)
                     (let ([left (node-left node)]
                           [right (node-right node)])
                       (print-node-color node port)
                       (when (not (null? left))
                         (print-node-color left port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key left)))
                       (when (not (null? right))
                         (print-node-color right port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key right)))

                     ))
               (rb-trees-root rb))
    (display "}\n" port)))

;; internal procedures
(define (binary-search-tree? rb)
  (define (rec node)
    (if (null? node)
        '()
        (append (rec (node-left node))
                (list (node-key node))
                (rec (node-right node)))))
  (let ([all-keys (rec (rb-trees-root rb))])
    (equal? (list-sort <= all-keys) all-keys)))
)
