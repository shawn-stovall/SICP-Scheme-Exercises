#lang scheme
(define (cont-frac n d k)
  (define (iter result count)
    (let ((n-value (n count))
          (d-temp (d (- count 1))))
      (if (= count 1)
          (/ n-value result)
          (iter (+ d-temp (/ n-value result))  (- count 1)))))
  (iter (d k) k))

(define (square x)
  (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))