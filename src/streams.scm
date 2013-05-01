;;
;; streams.scm
;;

(define (clist lo hi)
  (if (>= lo hi) nil
      (cons lo (clist (+ lo 1) hi))))

(clist 5 15)
