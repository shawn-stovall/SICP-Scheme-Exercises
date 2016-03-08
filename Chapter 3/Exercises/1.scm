#lang scheme
(define (make-accumulator x)
  (lambda (acc)
    (set! x (+ x acc))
    x))