;;
;; recur.scm
;;

(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (iter product counter)
    (println "iter=" counter ", product=" product) 
    (if (> counter n)
	product
	(iter (* counter product) (+ counter 1))))
  (iter 1 1))

(factorial 6)

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 90)

;; count change

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (println "a=" amount ", k=" kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 4)

;; exponentiation

;; recursive
(define (exp b n)
  (if (= n 0) 1
      (* b (exp b (- n 1)))))

;; iterative
(define (exp b n)
  (define (loop accu n)
    (if (= n 0) accu
	(loop (* b accu) (- n 1))))
  (loop 1 n))

(exp 2 10)

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(fast-expt 2 10)

;; gcd

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(gcd 206 40)

;; primes

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(newline)

;; higher order

(define inc (lambda (x) (+ x 1)))

(inc 5)

;; f (x, y) = x(1 + xy)2 + y(1 − y) + (1 + xy)(1 − y)

(define (fn x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(fn 3 5)
