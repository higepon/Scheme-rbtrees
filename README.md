# What is this?
Red-black tree (http://en.wikipedia.org/wiki/Red-black_tree) implmentation for R6RS.
The implementation is based on "Introduction to Algorithms" by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein.
## Using Red-black tree
Put rbtree.ss to your LOADPATH.

    (import (rnrs) (rbtree))
    (let ([rb (make-rbtree string=? string<?)])
      (rbtree-get rb "abc" 'not-found) ;; => 'not-found
      (rbtree-set! rb "abc" 'val1)
      (rbtree-get rb "abc") ;; => 'val1
      (rbtree-delete! rb "abc")
      (rbtree-get rb "abc" 'not-found)) ;; => 'not-found

## API
- (make-rbtree key=? key<?)
  - Creates and returns an rbtree. The arguments key=? and key<? are both procedures that take two arguments, which are the keys. The key=? procedure should return #t if two arguments are equivalent, or #f otherwise. The key<? procedure should return #t if the first argument is strictly less than the second argument, or #f otherwise.
- (rbtree? obj)
  - Returns #t if obj is rbtree, otherwise #f.
- (rbtree-set! rbtree key value)
  - Inserts an entry with a key and corresponding value into rbtree. If there already exists an entry with a key which is equivalent (under key=?), the entry is modified to have value.
- (rbtree-delete! rbtree key)
  - Deletes an entry with key from rbtree if such an entry exists, and returns #t otherwise #f.
- (rbtree-get rbtree key . fallback) 
  - Returns value associated with key. If key is not found and fallback is specified, returns fallback, otherwise raise error.
- (rbtree-size rbtree)
  - Returns number of elementes.
- (rbtree-contains? rbtree key)
  - Returns #t if rbtree has value associated with key, otherwise #f.
- (rbtree-keys rb)
  - Returns keys in ascending order of the keys.

## Author
Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>

## License
New BSD License

