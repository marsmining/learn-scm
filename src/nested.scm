;;
;; nested.scm
;;

;; foo lib
;;

(define (range a b)
  (if (= a b) '()
      (cons a (range (+ a 1) b))))

(define nil '())

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

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

(define (square n) (* n n))

;; nested sicp
;;

(define (combos n)
  (flatmap (lambda (i)
	 (map (lambda (j) (list i j))
	      (range 1 i)))
       (range 1 (+ n 1))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (combos n))))

(combos 6)

(prime-sum-pairs 6)

;; permutations
;;

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)     ; emptyset?
      (list nil)    ; sequence containing emptyset
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

;; forech item, cons item with each set of permutations
;; of the set minus the current item, so, we get eg:
;;
;; 2-4: (2 4) (4 2)
;; 2-6: (2 6) (6 2)
;; 4-6: (4 6) (6 4)
;;
(permutations (list 2 4 6))
(permutations (list 2 4))


