#lang scheme
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            (else (set! count (+ count 1))
                  (f x))))))