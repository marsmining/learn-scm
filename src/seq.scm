;;
;; seq.scm
;;

(define seq (cons 1 (cons 2 (cons 3 (cons 4 '())))))

(list 1 2 3 4 5 6)

;; range recursive
(define (range a b)
  (define (iter n)
    (if (= n b) '()
	(cons n (iter (+ n 1)))))
  (iter a))

;; find recursive
(define (find xs n)
  (if (= n 0) (car xs)
      (find (cdr xs) (- n 1))))

;; length recursive
(define (len xs)
  (if (null? xs) 0
      (+ (len (cdr xs)) 1)))

;; length iterative
(define (len-iter xs)
  (define (loop ys accu)
    (if (null? ys) accu
	(loop (cdr ys) (+ accu 1))))
  (loop xs 0))

(find (range 0 100) 99)

(len seq)

(len-iter (range 33 66))

;; append two lists
(define (append xs ys)
  (if (null? xs) ys
      (cons (car xs) (append (cdr xs) ys))))


(append (range 20 30) seq)

(define (mymap xs fn)
  (if (null? xs) '()
      (cons (fn (car xs)) (mymap (cdr xs) fn))))

(define (sq n) (* n n))

(map (range 0 5) (lambda (n) (+ n n)))

(define (isEqual xs ys)
  (cond ((null? xs) (null? ys))
	((null? ys) (null? xs))
	((= (car xs) (car ys)) (isEqual (cdr xs) (cdr ys)))
	(else (= 1 0))))

(isEqual (range 0 4) (list 1 2 3 4))

;; trees

(define x (cons (list 1 2) (range 3 8)))

(define (count-leaves xs)
  (cond ((null? xs) 0)
	((not (pair? xs)) 1)
	(else
	 (+ (count-leaves (car xs))
	    (count-leaves (cdr xs))))))

(count-leaves x)

(define (tree-map xs fn)
  (cond ((null? xs) '())
	((not (pair? xs)) (fn xs))
	(else
	 (cons (tree-map (car xs) fn)
	       (tree-map (cdr xs) fn)))))

(tree-map x (lambda (n) (* n n)))

;; filter

(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(filter even? (range 0 50))

;; accu

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (sq n) (* n n))
(map sq (range 0 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 10 11))

(define (length sequence) (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (range 5 34))

