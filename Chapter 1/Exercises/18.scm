#lang scheme
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (mult n m)
  (mult-iter n m 0))

(define (mult-iter n m a)
    (cond ((= n 0) a)
          ((even? n) (mult-iter (halve n) (double m) a))
          (else (mult-iter (- n 1) m (+ m a)))))