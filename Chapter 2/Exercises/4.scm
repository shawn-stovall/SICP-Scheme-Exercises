#lang scheme
(define (o-cons x y)
  (lambda (m) (m x y)))

(define (o-car z)
  (z (lambda (p q) p)))

(define (o-cdr z)
  (z (lambda (p q) q)))