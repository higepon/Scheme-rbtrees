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
   rb-delete!
   rb-get
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
       (c #f)))))

(define-record-type node
  (fields
   (mutable left)
   (mutable right)
   (mutable parent)
   (mutable key)
   (mutable value)
   (mutable color)))

(define (rb-get rb key . fallback)
  (let ([node (rb-get-node (rb-trees-root rb) key)])
    (cond
     [node (node-value node)]
     [else
      (if (pair? fallback)
          (car fallback)
          (error 'rb-get (format "key = ~a not found" key) key))])))

(define (rb-get-node node key)
  (cond
   [(not node) #f]
   [(= key (node-key node))
    node]
   [(> key (node-key node))
    (rb-get-node (node-right node) key)]
   [else
    (rb-get-node (node-left node) key)]))

(define (rb-delete! rb key)
  (let ([node (rb-get-node (rb-trees-root rb) key)])
    (when node
        (node-delete! rb node))))

(define (tree-successor x)
  (cond
   [(node-right x)
    (tree-minimum (node-right x))]
   [else
    (let ([y (node-parent x)])
      (let loop ([x x]
                 [y y])
        (cond
         [(or y (not (eq? x (node-right y))))
          y]
         [else
          (loop y (node-parent y))])))]))

(define (tree-minimum x)
  (if (node-left x)
      (tree-minimum (node-left x))
      x))

(define (rb-delete-fixup rb x)
  (let loop ([x x]
             [w '()])
    (cond
     [(or (eq? x (rb-trees-root rb)) (red? x))
      (node-color-set! x 'black)]
     [else
      (cond
       [(eq? x (node-left (node-parent x)))
        (set! w (node-right (node-parent x)))
        (when (red? w)
          (node-color-set! w 'black)
          (node-color-set! (node-parent x))
          (left-rotate rb (node-parent x))
          (set! w (node-right (node-parent x))))
        (cond
         [(and (black? (node-left w)) (black? (node-right w)))
          (node-color-set! w 'red)
          (set! x (node-parent x))]
         [(black? (node-right w))
          (node-color-set! (node-left w) 'black)
          (node-color-set! w 'red)
          (right-rotate rb w)
          (set! w (node-right (node-parent x)))]
         [else '()])
        (node-color-set! w (node-color (node-parent x)))
        (node-color-set! (node-parent x) 'black)
        (node-color-set! (node-right w) 'black)
        (left-rotate rb (node-parent x))
        (set! x (rb-trees-root rb))
        (loop x w)]
       [else
        (set! w (node-left (node-parent x)))
        (when (red? w)
          (node-color-set! w 'black)
          (node-color-set! (node-parent x))
          (right-rotate rb (node-parent x))
          (set! w (node-left (node-parent x))))
        (cond
         [(and (black? (node-right w)) (black? (node-left w)))
          (node-color-set! w 'red)
          (set! x (node-parent x))]
         [(black? (node-left w))
          (node-color-set! (node-right w) 'black)
          (node-color-set! w 'red)
          (left-rotate rb w)
          (set! w (node-left (node-parent x)))]
         [else '()])
        (node-color-set! w (node-color (node-parent x)))
        (node-color-set! (node-parent x) 'black)
        (node-color-set! (node-left w) 'black)
        (right-rotate rb (node-parent x))
        (set! x (rb-trees-root rb))
        (loop x w)
        ]
       )])))

(define (node-delete! rb z)
  (let* ([y (if (or (not (node-left z)) (not (node-right z)))
                z
                (tree-successor z))]
         [x (if (node-left y) (node-left y) (node-right y))])
    (node-parent-set! x (node-parent y))
    (cond
     [(not (node-parent y))
      (rb-trees-root-set! rb x)]
     [(eq? y (node-left (node-parent y)))
      (node-left-set! (node-parent y) x)]
     [else
      (node-right-set! (node-parent y) x)])
    (when (not (eq? y z))
      (node-key-set! z (node-key y))
      (node-value-set! z (node-value y))
      (node-color-set! z (node-color y)))
    (when (black? y)
      (rb-delete-fixup rb x))
    y))

(define (rb-set! rb key value)
  (let loop ([x (rb-trees-root rb)]
             [y #f])
    (cond
     [(not x)
      (let ([z (make-node #f #f y key value 'black)])
;        (format #t "set key=~a y=~a\n" key y)
        (cond
         [(not y)
          (node-color-set! z 'black)
          (rb-trees-root-set! rb z)]
         [(< key (node-key y))
          (node-color-set! z 'red)
          (node-left-set! y z)]
         [else
          (node-color-set! z 'red)
          (node-right-set! y z)])
        (insert-fixup rb z))]
     [else
      (if (< key (node-key x))
          (loop (node-left x) x)
          (loop (node-right x) x))])))

(define (node-fold init proc node)
  (cond
   [(not node)
    init]
   [else
    (let ([accum (proc init node)])
      (node-fold (node-fold accum proc (node-left node)) proc (node-right node)))]))


(define (check-rb rb)
  (define (raise-error reason)
    (error 'check-rb reason))
  (assert (rb-trees? rb))
  (cond
   [(not (rb-trees-root rb)) #t]
   [else
    (and (or (binary-search-tree? rb)
             (raise-error "not binary-search-tree"))
         (or (black? (rb-trees-root rb))
             (raise-error "root is not black"))
         (or (red-has-two-black? (rb-trees-root rb))
             (raise-error "red should have black childlen"))
         (or (node-fold #t (lambda (accum node) (and accum (black-hight-same? node))) (rb-trees-root rb))
             (raise-error "black height should be same")))]))

(define (leaf? node)
  (and (not (node-left node))
       (not (node-right node))))

(define (black? node)
  (eq? 'black (node-color node)))

(define (red? node)
  (eq? 'red (node-color node)))

(define (left-rotate rb x)
  (let ([y (node-right x)])
    (node-right-set! x (node-left y))
    (when (node-left y)
      (node-parent-set! (node-left y) x))
    (node-parent-set! y (node-parent x))
    (if (not (node-parent x))
        (rb-trees-root-set! rb y)
        (if (eq? x (node-left (node-parent x)))
            (node-left-set! (node-parent x) y)
            (node-right-set! (node-parent x) y)))
    (node-left-set! y x)
    (node-parent-set! x y)))

(define (right-rotate rb x)
  (let ([y (node-left x)])
    (node-left-set! x (node-right y))
    (when (node-right y)
      (node-parent-set! (node-right y) x))
    (node-parent-set! y (node-parent x))
    (if (not (node-parent x))
        (rb-trees-root-set! rb y)
        (if (eq? x (node-right (node-parent x)))
            (node-right-set! (node-parent x) y)
            (node-left-set! (node-parent x) y)))
    (node-right-set! y x)
    (node-parent-set! x y)))

(define (insert-fixup-rec rb z node-fetch node-rotate1 node-rotate2)
  (let ([y (node-fetch (node-parent (node-parent z)))])
    (cond
     [(and y (red? y))
      (node-color-set! (node-parent z) 'black)
      (node-color-set! y 'black)
      (node-color-set! (node-parent (node-parent z)) 'red)
      (insert-fixup rb (node-parent (node-parent z)))]
     [else
      (when (eq? z (node-fetch (node-parent z)))
        (set! z (node-parent z))
        (node-rotate1 rb z))
      (node-color-set! (node-parent z) 'black)
      (node-color-set! (node-parent (node-parent z)) 'red)
      (node-rotate2 rb (node-parent (node-parent z)))
      (insert-fixup rb z)])))

(define (insert-fixup rb z)
  (cond
   [(and (node-parent z) (red? (node-parent z)))
    (cond
     [(eq? (node-parent z) (node-left (node-parent (node-parent z))))
      (insert-fixup-rec rb z node-right left-rotate right-rotate)]
     [else
      (insert-fixup-rec rb z node-left right-rotate left-rotate)])]
   [else '()])
  (node-color-set! (rb-trees-root rb) 'black))

(define (red-has-two-black? node)
  (cond
   [(not node) #t]
   [(red? node)
    (and (or (not (node-left node)) (black? (node-left node)))
         (or (not (node-right node)) (black? (node-right node)))
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
    (let ([h (if (black? node) (+ h 1) h)])
      (cond
       [(and (not (node-left node)) (not (node-right node)))
        (add-height! h)]
       [else
        (when (node-left node)
          (rec h (node-left node)))
        (when (node-right node)
          (rec h (node-right node)))])))
  (rec 0 node)
  (let ([height (car height*)])
    (for-all (lambda (x) (= height x)) height*)))

(define nil-index 0)
(define (gen-nil)
  (let ([x (format "Nil~d" nil-index)])
    (set! nil-index (+ nil-index 1))
    x))

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
                       (cond
                        [(not left)
                         (let ([nil (gen-nil)])
                           (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" nil)
                           (format port "    ~s -> ~s;\n" (node-key node) nil))]
                        [else
                         (print-node-color left port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key left))])
                       (cond
                        [(not right)
                         (let ([nil (gen-nil)])
                           (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" nil)
                           (format port "    ~s -> ~s;\n" (node-key node) nil))]
                        [else
                         (print-node-color right port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key right))])
                     ))
               (rb-trees-root rb))
    (display "}\n" port)))

;; internal procedures
(define (binary-search-tree? rb)
  (define (rec node)
    (if (not node)
        '()
        (append (rec (node-left node))
                (list (node-key node))
                (rec (node-right node)))))
  (let ([all-keys (rec (rb-trees-root rb))])
    (equal? (list-sort <= all-keys) all-keys)))
)
