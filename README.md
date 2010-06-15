# What is this?
Red-black tree (http://en.wikipedia.org/wiki/Red-black_tree).

# Using Red-black tree
Put rbtree.ss to your LOADPATH.

    (import (rnrs) (rbtree))
    (let ([rb (make-rbtree string=? string<?)])
      (rbtree-get rb "abc" 'not-found) ;; => 'not-found
      (rbtree-set! rb "abc" 'val1)
      (rbtree-get rb "abc") ;; => 'val1
      (rbtree-delete! rb "abc")
      (rbtree-get rb "abc" 'not-found)) ;; => 'not-found

## API
- (rbtree? obj) => #t or #f
- (rbtree-set! rb key value)
- (rbtree-keys rb) : Returns keys as list
- (rbtree-delete! rb key) : delete the value associated with key
- (rbtree-get rb key . fallback) : 
- (rbtree-size rb) : number of elements
- (rbtree-contains? rb key)
- (make-rbtree compare=? compare<?)

## Author
Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>

## License
New BSD License
