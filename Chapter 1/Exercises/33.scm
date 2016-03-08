#lang scheme
(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter? a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum-prime-squares a b)
  (define (next x)
    (+ x 1))
  (filtered-accumulate + 0 square a next b prime?))

(define (prorelprime n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (define (filter? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 term 1 next (- n 1) filter?))