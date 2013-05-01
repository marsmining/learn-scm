;;
;; tmp.scm
;;

(define (flatten xs)
  (define (loop accu rem)
    (cond ((null? rem) accu)
	  (else (cond ((atom? (car rem)) (loop accu (cdr rem)))
		      (else (cons (car rem) (flatten (cdr rem))))))))
  (loop '() xs))


;; flatten - if null?, return empty list
;;           else, if atom?, add to accu
;;                 else, create list of flatten + flatten

(define x (list 15 (list 3 (list 3 2 1) 9) 12))

x

(flatten x)

