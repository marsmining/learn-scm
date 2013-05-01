;;
;; sicp3-1.scm
;;

"---------------------------------------------"

(define balance 400)

(set! balance 300)

balance

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define wd (new-withdraw 59))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request - MAKE-ACCOUNT"
		       m))))
  dispatch)

(define acct (make-account 50))

((acct 'deposit) 50)

(define (make-accumulator n)
  (let ((accu n))
    (lambda (x)
      (begin (set! accu (+ x accu)) accu))))

(define A (make-accumulator 5))

(A 10)
(A 10)

;; exercise 3.2
;;
(define (sq n) (* n n))
(define (make-monitored f)
  (let ((cnt 0))
    (lambda (arg)
      (cond ((number? arg)
	     (set! cnt (+ cnt 1))
	     (apply f (list arg)))
	    ((eq? arg 'how-many-calls?)
	     cnt)
	    (else "unkown req")))))

(define s (make-monitored sq))

(s 100)

(s 'how-many-calls?)

;; 
