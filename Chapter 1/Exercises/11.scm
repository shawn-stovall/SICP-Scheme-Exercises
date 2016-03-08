#lang scheme
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (iter 2 1 0 n))

(define (iter n1 n2 n3 count)
    (if (< count 3)
        n1
        (iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- count 1))))