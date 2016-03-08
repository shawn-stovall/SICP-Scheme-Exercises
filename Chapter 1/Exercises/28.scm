#lang scheme
(define (square x)
  (* x x))

(define (check n m)
  (if (and (not (or (= n 1) (= n (- m 1))))
           (= (remainder (square n) m) 1))
      0
      (remainder (square n) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (fooling-fermat n)
  (define (fooling-fermat-iter counter)
    (if (= counter 0)
        #t
        (and (fermat-test n counter)
             (fooling-fermat-iter (- counter 1)))))
  (fooling-fermat-iter (- n 1)))