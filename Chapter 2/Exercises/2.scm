#lang scheme
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2.0)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2.0)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))