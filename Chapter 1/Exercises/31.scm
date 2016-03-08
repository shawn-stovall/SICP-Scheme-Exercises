#lang scheme
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product identity 1 next n))

(define (aprox-pi n)
  (define (next-top x)
    (if (even? x)
        (+ x 2)
        (+ x 1)))
  (define (next-bottom x)
    (if (even? x)
        (+ x 1)
        (+ x 2)))
  (define (next x)
    (+ x 1))
  (define (term x)
    (/ (next-top x)
       (next-bottom x)))
  (* 4.0 (product term 1 next n)))

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a) (product-recur term (next a) next b))))