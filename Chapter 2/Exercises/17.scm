#lang scheme
(define (last-pair l)
  (define (iter lst previous-element)
    (if (null? lst)
        previous-element
        (iter (cdr lst) (car lst))))
  (iter l (car l)))

(define (last-pair-recur l)
  (if (null? (cdr l))
      (car l)
      (last-pair-recur (cdr l))))