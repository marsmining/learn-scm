;;
;; lil_00.scm - following along the little schemer book
;;

;; ch. 1
;; toys

(define (atom? sexp)
  (and (not (pair? sexp)) (not (null? sexp))))

;; ch. 2
;; do it, do it again

(define (lat? xs)
  (cond ((null? xs) #t)
        ((atom? (car xs)) (lat? (cdr xs)))
        (else #f)))

(define (member? x xs)
  (cond ((null? xs) #f)
        (else (or (eq? x (car xs))
                  (member? x (cdr xs))))))

;; ch. 3
;; cons the magnificent

(define (o-rember x xs)
  (cond ((null? xs) '())
        ((eq? x (car xs)) (cdr xs))
        (else (cons (car xs) (o-rember x (cdr xs))))))

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

(define (numbered? aexp)
  (cond ((atom? aexp) (number? aexp))
        (else (and (numbered? (car aexp))
                   (numbered? (caddr aexp))))))

(numbered? '(5 + (3 ^ 7)))

(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (cadr nexp) '+)
         (+ (value (car nexp)) (value (caddr nexp))))
        ((eq? (cadr nexp) 'x)
         (* (value (car nexp)) (value (caddr nexp))))))

(value '(3 + 5))
(value '(3 x 5))
(value '(3 x (2 + 12)))

(define 1st-sub-expr cadr)
(define 2nd-sub-expr caddr)
(define operator car)

(define (valuep nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (operator nexp) '+)
         (+ (valuep (1st-sub-expr nexp)) (value (2nd-sub-expr nexp))))
        ((eq? (operator nexp) 'x)
         (* (valuep (1st-sub-expr nexp)) (value (2nd-sub-expr nexp))))))

(valuep '(+ (x 4 4) 8))

;; ch. 7
;; friends and relations

(define (set? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (set? (cdr lat)))))

(eq? #t (set? '(a b c d)))
(eq? #f (set? '(a b c d c)))
(eq? #f (set? '(a b c 5 6 b)))

;; using `member?`
(define (makeset lat)
  (cond ((null? lat) '())
        ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
        (else (cons (car lat) (makeset (cdr lat))))))

(equal? '(a b c d) (makeset '(a b c c d d)))

;; using `multirember`
(define (makesetp lat)
  (cond ((null? lat) '())
        (else (cons (car lat) (makesetp (multirember (car lat) (cdr lat)))))))

(equal? '(a 3 b c d) (makesetp '(a 3 a b 3 c a c d d)))

(define (subset? s1 s2)
  (cond ((null? s1) #t)
        (else (and (member? (car s1) s2)
                   (subset? (cdr s1) s2)))))

(eq? #t (subset? '(a b) '(e f b a)))
(eq? #f (subset? '(a c) '(e f b a)))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(eq? #t (eqset? '(a b c) '(c b a a b)))
(eq? #t (eqset? '(a b c) '(c b a a b)))
(eq? #f (eqset? '(a b c) '(c y b a a b)))

(define (intersect? s1 s2)
  (cond ((null? s1) #f)
        (else (or (member? (car s1) s2)
                  (intersect? (cdr s1) s2)))))

(eq? #t (intersect? '(a b c) '(c d e f)))
(eq? #f (intersect? '(a b c) '(d e f)))

(define (intersect s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2)
         (cons (car s1) (intersect (cdr s1) s2)))
        (else (intersect (cdr s1) s2))))

(equal? '(c d) (intersect '(a b c d e f) '(y d c x)))

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((member? (car s1) s2) (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2)))))

(equal? '(a b c d e f) (union '(a b c d) '(d e f)))

(define (difference s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2) (difference (cdr s1) s2))
        (else (cons (car s1) (difference (cdr s1) s2)))))

(equal? '(a b) (difference '(a b c d) '(c d e)))

(define (intersectall l-set)
  (cond ((null? (cdr l-set)) (car l-set))
        (else (intersect (car l-set)
                         (intersectall (cdr l-set))))))

;; evals to something like:
;; (isect car (isect car (isect car cdr)))

;; alt impl using reduce
(define (intersectallp l-set)
  (reduce-left intersect '() l-set))

(equal? '(d e) (intersectall '((a b c d e f g) (f e d y) (d e g e))))

(define (a-pair x)
  (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cddr x)) #t)
        (else #f)))

(eq? #t (a-pair '(a b)))
(eq? #t (a-pair '(5 6)))
(eq? #t (a-pair '((g b) (i g))))
(eq? #f (a-pair '(a b c)))

(define fst car)
(define snd cadr)

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (fun? rel)
  (set? (firsts rel)))

(eq? #t (fun? '((a b) (c d) (5 g))))
(eq? #f (fun? '((a b) (c d) (a g))))

(define (revpair p) (build (snd p) (fst p)))

(define (revrel rel)
  (cond ((null? rel) '())
        (else (cons
               (revpair (car rel))
               (revrel (cdr rel))))))

(revrel '((a b) (1 2)))

(define (fullfun? fun)
  (fun? (revrel fun)))

(define one-to-one? fullfun?)

(eq? #t (fullfun? '((a b) (x y))))
(eq? #f (fullfun? '((x y) (a b) (v y))))

;; ch. 8
;; lambda the ultimate

;; skipping basic higher order fn review..

;; but this guy looks strange. we're creating
;; a new function on each recursion. the new
;; fn closes over lat free variable.

(define (multirember-co a lat col)
  (cond
   ((null? lat) (col '() '()))
   ((eq? (car lat) a)
    (multirember-co
     a (cdr lat)
     (lambda (newlat seen)
       (col newlat (cons (car lat) seen)))))
   (else
    (multirember-co
     a (cdr lat)
     (lambda (newlat seen)
       (col (cons (car lat) newlat) seen))))))

(define (a-friend x y) (null? y))

(multirember-co 'tuna '() a-friend)
;; (a-friend '() '()) => #t

(multirember-co 'tuna '(tuna) a-friend)
;; eval steps
;; 1. call fn, eq? true, so
;; 2. (multirember-co 'tuna '() f), f is a new fn,
;;    of 2 args, which calls a-friend like..
;; 3. (a-friend '() '(tuna)) => #f

(multirember-co 'tuna '(wahoo tuna) a-friend)
;; eval steps
;; 1. eq? wahoo tuna => false
;; 2. (multirember-co 'tuna '(tuna) f), f is a new fn
;;    f calls a-friend with 2nd arg unchanged,
;;    and wahoo cons'd to 1st arg
;; 3. eq? tuna tuna => true
;; 4. (multirember-co 'tuna '() f), f is a new fn
;;    f is a fn, which calls previous steps fn
;;    1st arg unchanged, tuna cons'd to second arg
;; 5. null? lat => true
;; 6. call fn from previous step, with two empty lists
;;
;; end up with something like:
((lambda (a b)
   ((lambda (x y)
      (a-friend (cons 'wahoo x) y))
    a (cons 'tuna b)))
 '() '())

;; need to revisit this section of ch. 8
;; explore continuations more

;; ch. 9
;; ..and again, and again..

;; partial fn
(define (keep-looking a c lat)
  (cond ((number? c) (keep-looking a (pick c lat) lat))
        (else (eq? a c))))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (shift x)
  x)

(eq? '(a (b c)) (shift '((a b) c)))
