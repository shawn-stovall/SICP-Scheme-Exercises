#lang scheme
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define x-to-x-first
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               1.2))

(newline)

(define (average x y)
  (/ (+ x y) 2))

(define x-to-x-second
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               1.2))