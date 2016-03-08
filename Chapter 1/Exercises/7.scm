#lang scheme
(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess improved-guess)
  (< (/ (abs (- improved-guess guess)) improved-guess) 0.00000001))

(define (o-sqrt x)
  (sqrt-iter 1.0 x))