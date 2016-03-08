#lang scheme
(define (o-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (o-car x)
  (define (iter x count)
    (if (not (= (remainder x 2) 0))
        count
        (iter (/ x 2) (+ count 1))))
  (iter 0))

(define (o-cdr x)
  (define (iter x count)
    (if (not (= (remainder x 3) 0))
        count
        (iter (/ x 3) (+ count 1))))
  (iter x 0))