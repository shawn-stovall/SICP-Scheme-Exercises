#lang scheme
(define (inc x)
  (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0)
                  (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (* (/ h 3) (sum term 0 inc n)))