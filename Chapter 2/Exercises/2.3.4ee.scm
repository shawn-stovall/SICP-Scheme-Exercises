#lang scheme
(require racket/trace)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (eq? (symbol-leaf tree) symbol)
             '()
             (error "symbol not found -- ENCODE-SYMBOL: " symbol)))
        ((symbol-of? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((symbol-of? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not found -- ENCODE-SYMBOL:" symbol))))

(define (symbol-of? symbol set)
  (if (null? set)
      false
      (or (eq? symbol (car set))
          (symbol-of? symbol (cdr set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge freq-pairs)
  (cond ((null? (cdr freq-pairs)) (car freq-pairs))
        (else
         (let ((a (car freq-pairs))
               (b (cadr freq-pairs))
               (rest (cddr freq-pairs)))
           (successive-merge (adjoin-set (make-code-tree a b)
                                         rest))))))

(trace successive-merge)

(define song-tree
  (generate-huffman-tree
   '((Wah 1) (boom 1) (a 2) (Get 2) (job 2) (Sha 3) (yip 9) (na 16))))

(define encoded-message
  (encode '(Get a job Sha na na na na na na na na Get a job Sha
                na na na na na na na na Wah yip yip yip yip yip yip yip yip yip)
          song-tree))