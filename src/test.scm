

(define (square x) (* x x))

(square 5)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(abs 33)

(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))


      


(define (sqrt x)
  (define (isGoodEnough g)
    (< (abs (- x (square g))) .01))
  (define (sqrt-iter g)
    (if (isGoodEnough g) g
	(sqrt-iter (/ (+ g (/ x g)) 2))))
  (sqrt-iter 1))

(sqrt 9.0)
