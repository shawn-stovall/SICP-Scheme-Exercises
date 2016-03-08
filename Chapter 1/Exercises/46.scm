#lang scheme
(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a
        b)
     2))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt-iter x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.000000001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define tolerance 0.00001)
(define (fixed-point f)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) tolerance))
    (lambda (guess)
      (f guess)))
   1.0))