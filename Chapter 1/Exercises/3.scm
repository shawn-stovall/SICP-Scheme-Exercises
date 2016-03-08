#lang racket
(define (sum-of-squares a b c)
  (cond
    ((and (> a b) (> b c)) (+ (* a a) (* b b)))
    ((and (< a c) (> b c)) (+ (* b b) (* c c)))
    ((and (< a b) (< b c)) (+ (* b b) (* c c)))
    (else (+ (* a a) (* c c)))))