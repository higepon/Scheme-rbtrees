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
- (rbtree? obj)
  - Returns #t when obj is rbtree, otherwise #f.
- (rbtree-set! rb key value)
  - Set (key value) to the rbtree.
- (rbtree-keys rb)
  - Returns keys as list in desc order.
- (rbtree-delete! rb key)
  - Delete (key value) from the rb. When key is not found, do nothing.
- (rbtree-get rb key . fallback) 
  - Returns value associated with key.  When key is not found and fallback is specified, returns fallback, otherwise raise error.
- (rbtree-size rb) : number of elements
  - Returns number of key.
- (rbtree-contains? rb key)
  - Returns #t when the rb
- (make-rbtree compare=? compare<?)

## Author
Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>

## License
New BSD License
