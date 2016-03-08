#lang scheme
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (mult n m)
  (cond ((= n 0) 0)
        ((even? n) (double (mult (halve n) m)))
        (else (+ m (mult (- n 1) m)))))