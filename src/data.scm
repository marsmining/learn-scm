;;
;; data.scm
;;

(define (yell n)
  (if (= n 0) "hiya"
      (string-append (yell (- n 1)) "a")))

(yell 5)

;; rational numbers

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(print-rat one-half)

;; cons, car, cdr

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 - CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define hmm (cons 11 12))
(define hmmm (cons 11 12))

(cdr hmm)

