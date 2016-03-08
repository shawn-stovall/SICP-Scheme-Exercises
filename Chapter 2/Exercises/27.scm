#lang scheme
(define (deep-reverse l)
  (define (iter l result)
    (cond ((null? l) result)
          ((not (pair? (car l))) (iter (cdr l) (cons (car l) result)))
          (else (iter (cdr l) (cons (iter (car l) '()) result)))))
  (iter l '()))

(define x (list (list 1 2) (list 3 4)))