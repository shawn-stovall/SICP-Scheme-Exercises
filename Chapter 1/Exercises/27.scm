#lang scheme
(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (fooling-fermat n)
  (define (fooling-fermat-iter counter)
    (if (= counter 0)
        #t
        (and (fermat-test n counter)
             (fooling-fermat-iter (- counter 1)))))
  (fooling-fermat-iter (- n 1)))