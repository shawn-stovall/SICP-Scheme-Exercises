#lang scheme
(define (cont-frac n d k)
  (define (iter result count)
    (let ((n-value (n count))
          (d-temp (d (- count 1))))
      (if (= count 1)
          (/ n-value result)
          (iter (+ d-temp (/ n-value result))  (- count 1)))))
  (iter (d k) k))

(define (e-expansion acc)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (x)
                    (let ((y (+ x 1)))
                      (if (not (= (remainder y 3) 0))
                          1
                          (* 2 (/ y 3)))))
                  acc)))