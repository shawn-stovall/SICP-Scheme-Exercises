#lang scheme
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a
        b)
     2))

(define (square x)
  (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y)))))
               1.0))

(define (fifth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y y)))))
               1.0))

(define (sixth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/  x (* y y y y y)))))
               1.0))

(define (eleventh-root x)
  (fixed-point (average-damp (average-damp (average-damp
                                            (lambda (y)
                                              (/ x
                                                 (* y y y y y y y y y y))))))
               1.0))

(define (twelth-root x)
  (fixed-point (average-damp (average-damp (average-damp
                                            (lambda (y)
                                              (/ x
                                                 (* y y y y y y y y y y y))))))
               1.0))

(define (fifteenth-root x)
  (fixed-point (average-damp (average-damp (average-damp
                                            (lambda (y)
                                              (/ x
                                                 (expt y 14))))))
               1.0))

(define (sixteenth-root x)
  (fixed-point (average-damp (average-damp (average-damp
                                            (lambda (y)
                                              (/ x
                                                 (expt y 15))))))
               1.0))

(define (seventh-root x)
  (fixed-point (average-damp (average-damp (lambda (y)
                                             (/ x (* y y y y y y)))))
               1.0))

(define (eighth-root x)
  (fixed-point (average-damp (average-damp (lambda (y)
                                             (/ x (* y y y y y y y)))))
               1.0))

(define (logB b x)
  (/ (log x) (log b)))

(define (root nth b)
  (fixed-point ((repeated average-damp
                          (floor (logB 2 nth)))
                (lambda (y)
                  (/ b (expt y (- nth 1)))))
               1.0))