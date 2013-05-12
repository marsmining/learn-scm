;;
;; reasoned.scm - following along the reasoned schemer book
;;

(load "kanren-book/mk.scm")
(load "kanren-book/mkextraforms.scm")
(load "kanren-book/mkprelude.scm")

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



(restart 1)
