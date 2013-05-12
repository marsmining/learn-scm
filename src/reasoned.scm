;;
;; reasoned.scm - following along the reasoned schemer book
;;

(load "kanren-book/mk.scm")
(load "kanren-book/mkextraforms.scm")
(load "kanren-book/mkprelude.scm")

(run*
 (q)
 (== #t q))

(restart 1)
