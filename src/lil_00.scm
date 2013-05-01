;;
;; lil_00.scm - first part of little schemer book
;;

(define (atom? sexp)
  (and (not (pair? sexp)) (not (null? sexp))))

(define (lat? xs)
  (cond ((null? xs) #t)
	((atom? (car xs)) (lat? (cdr xs)))
	(else #f)))

(define (member? x xs)
  (cond ((null? xs) #f)
	(else (or (eq? x (car xs))
		  (member? x (cdr xs))))))

(define (rember x xs)
  (cond ((null? xs) '())
	((eq? x (car xs)) (cdr xs))
	(else (cons (car xs) (rember x (cdr xs))))))

(define (list-equal? xs ys)
  (cond ((null? xs) (null? ys))
	((null? ys) (null? xs))
	((eq? (car xs) (car ys)) (list-equal? (cdr xs) (cdr ys)))
	(else #f)))


(define (firsts xs)
  (cond ((null? xs) '())
	(else (cons (car (car xs))
		    (firsts (cdr xs))))))

(define (insertR new old lat)
  (cond ((null? lat) '())
        (else
         (cond
          ((eq? (car lat) old)
           (cons old (cons new (cdr lat))))
          (else
           (cons (car lat)
                 (insertR new old (cdr lat))))))))

(define (insertL new old lat)
  (cond ((null? lat) '())
        (else
         (cond
          ((eq? (car lat) old)
           (cons new lat))
          (else
           (cons (car lat)
                 (insertL new old (cdr lat))))))))

(define (subst new old lat)
  (cond ((null? lat) '())
        (else
         (cond
          ((eq? (car lat) old)
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst new old (cdr lat))))))))

(define (multirember x xs)
  (cond ((null? xs) '())
        ((eq? x (car xs)) (multirember x (cdr xs)))
	(else (cons (car xs) (multirember x (cdr xs))))))

;; ch. 4
;; number games

(define (myplus a b)
  (cond ((zero? a) b)
        (else (myplus (sub1 a) (add1 b)))))

(define (addtup tup)
  (cond ((null? tup) 0)
        (else (+ (car tup) (addtup (cdr tup))))))

(define (xo a b)
  (cond ((= b 1) a)
        (else (+ a (xo a (sub1 b))))))

(define (tup+ tup1 tup2)
  (cond ((and (null? tup1) (null? tup2)) '())
        (else
         (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(define (gt a b)
  (cond
   ((zero? a) #f)
   ((zero? b) #t)
   (else (gt (sub1 a) (sub1 b)))))

(define (expo n f)
  (cond ((zero? f) 1)
        (else (* n (expo n (sub1 f))))))

(define (lng lat)
  (cond ((null? lat) 0)
        (else (add1 (lng (cdr lat))))))

(define (pick n lat)
  (cond ((zero? (sub1 n)) (car lat))
        (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

;; ch. 5
;; full of stars

(define (rember* a l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? a (car l)) (rember* a (cdr l)))
               (else (cons (car l) (rember* a (cdr l))))))
        (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
               (else (occur* a (cdr l)))))
        (else (+ (occur* a (car l))
                 (occur* a (cdr l))))))

(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
        (else (or (member* a (car l)) (member* a (cdr l))))))

(define myxs '(foo (bar cup) god (foo cup cup) (((cup)) doo)))

;; ch. 6
;; shadows

(define (numbered? l)
  l)
