;;
;; knuth.scm
;;

;; euclid gcd
;;
(define (gcd m n)
  (if (zero? (modulo m n)) n
      (gcd n (modulo m n))))



(gcd 256 38)

(trace gcd)
(display "-------------")
(gcd 119 544)
