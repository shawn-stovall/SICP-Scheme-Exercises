#lang scheme
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

#| 
   It works becuase when you hit the bottom of the list, it then cons the null list to
   a new list which is the second to last element of the original list mapped onto
   the null list.  This new pair then gets sent up to the next call as -rest- and then
   the new -rest- is cons'ed unto the new list which is the third from last element
   mapped to -rest-.
|#