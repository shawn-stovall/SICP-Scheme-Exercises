#lang scheme
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 y1 x2 y2 trials)
  (* (monte-carlo trials (p x1 y1 x2 y2))
     (area x1 y1 x2 y2)
     1.0))

(define (area x1 y1 x2 y2)
  (* (- x2 x1) (- y2 y1)))

(define (in-unit-circle? x1 y1 x2 y2)
  (lambda ()
    (<= (+ (sqr (+ (* (random) 2) -1))
           (sqr (+ (* (random) 2) -1)))
        1)))