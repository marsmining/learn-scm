;;
;; nine.scm
;;

;; import
;;
(load "lil_00.scm")

;; vars for testing
;;

(define x0 (list 1 2 3))
(define x1 (list 1 2 3 (list 4 5 6)))
(define x2 (list 15 (list 3 (list 3 2 1) 9) 12 (list 4 3)))
(define y0 (list 'a 'a 'a 'a 'b 'c 'c 'a 'a 'd 'e 'e 'e 'e))
(define z0 (list 'a 'b 'c 'c 'd))
(define z1 (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k))

;; p07 - flatten
;;
(define (flatten xs)
  (cond ((null? xs) '())
	((atom? (car xs)) (cons (car xs) (flatten (cdr xs))))
	(else (append (flatten (car xs)) (flatten (cdr xs))))))

x0 (flatten x0)
x1 (flatten x1)
x2 (flatten x2)

;; p08 - compress
;;
(define (compress xs)
  (fold-right (lambda (y ys)
		(cond ((null? ys) (cons y ys))
		      ((eq? y (car ys)) ys)
		      (else (cons y ys))))
	      '() xs))

y0 (compress y0)

;; (fold-right kons knil lis) = (kons (car lis) (fold-right kons knil (cdr lis)))

;; p09 - pack
;;
(define (pack xs)
  (define (red y ys)
    (if (and (not (null? ys)) (eq? y (caar ys)))
	(cons (cons y (car ys)) (cdr ys))
	(cons (list y) ys)))
  (fold-right red '() xs))

y0 (pack y0)

;; p10 - encode
;;
(define (encode xs)
  (map (lambda (e)
	 (cons (length e) (car e)))
       (pack xs)))

y0 (encodeUsingPack y0)

;; p11 - encodeModified
;;
(define (encodeModified xs)
  (map (lambda (e)
	 (if (= 1 (car e))
	     (cdr e)
	     e))
       (encode xs)))

y0 (encodeModified y0)

;; p12 - decode
;;
(define (decode xs)
  (flatten (map expand xs)))

(define (expand tup)
  (if (= (car tup) 0)
      '()
      (cons (cdr tup) (expand (cons (- (car tup) 1) (cdr tup))))))

(expand (cons 4 'a))

(encode y0) (decode (encode y0))

;; p13 - encodeDirect
;;
(define (encodeDirect xs)
  (define (red y ys)
    (if (and (not (null? ys)) (eq? y (caar ys)))
	(cons (cons y (+ 1 (cdar ys))) (cdr ys))
	(cons (cons y 1) ys)))
  (fold-right red '() xs))

y0 (encodeDirect y0)

;; p14 - duplicate
;;
(define (duplicate xs)
  (flatten (map (lambda (a b)
	 (list a b)) xs xs)))

z0 (duplicate z0)

;; p15 - duplicateN
;;
(define (duplicateN n xs)
  (flatten (map (lambda (e) (expand (cons n e))) xs)))

z0 (duplicateN 4 z0)

;; p16 - drop
;;
(define (drop n xs)
  (define (loop cnt rem)
    (cond ((null? rem) '())
	  ((zero? (modulo cnt n)) (loop (1+ cnt) (cdr rem)))
	  (else (cons (car rem) (loop (1+ cnt) (cdr rem))))))
  (loop 1 xs))

z1 (drop 3 z1)

;; p17 - split
;;
(define (split n xs)
  (define (loop accu rem cnt)
    (cond ((zero? cnt) (cons (reverse accu) (cdr rem)))
	  (else (loop (cons (car rem) accu) (cdr rem) (-1+ cnt)))))
  (loop '() xs n))

z1 (split 4 z1)

;; p18 - slice
;;
(define (sliceA i k xs)
  (define (loop accu rem cnt)
    (cond ((= cnt k) (reverse accu))
	  ((and (>= cnt i) (< cnt k))
	   (loop (cons (car rem) accu) (cdr rem) (+ cnt 1)))
	  (else (loop accu (cdr rem) (+ cnt 1)))))
  (loop '() xs 0))

(define (sliceB i k xs)
  (cond ((null? xs) '())
	((< k 1) '())
	((> i 0) (sliceB (- i 1) (- k 1) (cdr xs)))
	(else (cons (car xs) (sliceB i (- k 1) (cdr xs))))))

z1 (sliceB 3 7 z1)

;; p19 - rotate
;;
(define (rotate n xs)
  (define (loop accu rem cnt)
    (if (zero? cnt) (append rem (reverse accu))
	(loop (cons (car rem) accu) (cdr rem) (- cnt 1))))
  (loop '() xs n))

(define (rotateB n xs)
  (if (zero? n) xs
      (rotateB (- n 1) (append (cdr xs) (list (car xs))))))

z1 (rotateB 3 z1)

;; p20 - removeAt
;;
(define (removeAtOnly n xs)
  (if (zero? n) (car xs)
      (removeAt (- n 1) (cdr xs))))

(define (removeAt n xs)
  (define (loop accu rem n)
    (if (zero? n) (cons (car rem) (append (reverse accu) (cdr rem)))
	(loop (cons (car rem) accu) (cdr rem) (- n 1))))
  (loop '() xs n))

z1 (removeAt 4 z1)

;; p21 - insertAt
;;
(define (insertAt s n xs)
  (cond ((null? xs) '())
	((= n 0) (cons s xs))
	(else (cons (car xs) (insertAt s (- n 1) (cdr xs))))))

z1 (insertAt 'new 1 z1)

;; p22 - range
;;
(define (range a b)
  (if (= a b) '()
      (cons a (range (+ a 1) b))))

(range 4 14)

;; p23 - randomSelect
;;
(define (randomSelect n xs)
  (let ((plucked (removeAt (random (length xs)) xs)))
    (if (= n 0) '()
	(cons (car plucked) (randomSelect (- n 1) (cdr plucked))))))

z1 (randomSelect 3 z1)

;; p24 - lotto
;;
(define (lotto num-picks max)
  (randomSelect num-picks (range 0 max)))

(lotto 6 49)

;; p25 - randomPermute
;;
(define (randomPermute xs)
  (if (null? xs) '()
      (let ((plucked (removeAt (random (length xs)) xs)))
	(cons (car plucked) (randomPermute (cdr plucked))))))

z1 (randomPermute z1)

;; p26 - combinations
;;
(define (combinationsA n xs)
  (map (lambda (x)
	 (map (lambda (y)
		(map (lambda (z)
		       (list x y z)) xs))
	      xs))
       xs))

(define (combinations n xs)
  (define (addCombo)))
(combinations 3 '(a b c d e f))

;; monads
;;
(define (return v)
  (lambda (s) (cons v s)))

(define (bind mv f)
  (lambda (s)
    (let ((p (mv s)))
      ((f (car p)) (cdr p)))))

(define (take-sugar mv)
  (bind mv (lambda (v)
	     (lambda (s) (cons v (- s 1))))))

((take-sugar (take-sugar (return "me"))) 10)



