#lang scheme
(define (o-reverse l)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst) (cons (car lst) result))))
  (iter l '()))