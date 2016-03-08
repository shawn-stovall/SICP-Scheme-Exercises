#lang scheme
(define (square x)
  (* x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (cbrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (good-enough? guess improved-guess)
  (< (/ (abs (- improved-guess guess)) improved-guess) 0.0000000001))

(define (cbrt x)
  (cbrt-iter 1.0 x))