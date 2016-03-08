#lang scheme
(define f
  (let ((x 0))
    (lambda (y)
      (if (zero? x)
          (begin (set! x 1)
                 -1)
          (begin (set! x 1)
                 y)))))