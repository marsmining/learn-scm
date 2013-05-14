;;
;; reasoned.scm - following along the reasoned schemer book
;;

(load "kanren-book/mk.scm")
(load "kanren-book/mkextraforms.scm")
(load "kanren-book/mkprelude.scm")

;; ch. 1
;; playthings

(run*
 (q)
 (== #t q))

(run* (q)
      (fresh (x)
             (== #t x)
             (== x q)))

(run* (x) succeed) ; => (_.0)

(run* (r)
      (fresh (x y)
             (== (cons x (cons y '())) r)))
;; => ((_.0 _.1))

(run* (x)
      (conde ((== 'olive x) succeed)
             ((== 'oil x) succeed)))
;; => (olive oil)

(define (teacupo x)
  (conde ((== 'tea x) succeed)
         ((== 'cup x) succeed)
         (else fail)))

(run* (x) (teacupo x)) ; => (tea cup)

(run* (r)
      (fresh (x y)
             (conde ((teacupo x) (== #t y) succeed)
                    ((== #f x) (== #t y))
                    (else fail))
             (== (cons x (cons y '())) r)))
;; => ((tea #t) (cup #t) (#f #t))

;; ch. 2
;; teaching old toys new tricks



(restart 1)
