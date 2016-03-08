#lang scheme
(define (same-parity . x)
  (if (even? (car x))
      (all-evens x)
      (all-odds x)))

(define (all-cond test?)
  (lambda (l)
    (cond ((null? l) '())
          ((test? (car l)) (cons (car l) ((all-cond test?) (cdr l))))
          (else ((all-cond test?) (cdr l))))))

(define all-evens (all-cond even?))
(define all-odds  (all-cond odd?))