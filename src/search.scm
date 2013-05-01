;;
;; search.scm
;;

(define binary-search
  (lambda (vec sought . opt)
    (let ((precedes? (if (null? opt) < (car opt))))
      (let loop ((start 0)
                 (stop (- (vector-length vec) 1)))
        (if (< stop start)
            #f
            (let* ((midpoint (quotient (+ start stop) 2))
                   (mid-value (vector-ref vec midpoint)))
              (cond ((precedes? sought mid-value)
                     (loop start (- midpoint 1)))
                    ((precedes? mid-value sought)
                     (loop (+ midpoint 1) stop))
                    (else midpoint))))))))

(define v0 (vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(define v1 (vector 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm))

v0 (binary-search v0 7)


(define (bsearch vec sought)
  (let loop ((start 0) (stop (-1+ (vector-length vec))))
    (if (< stop start) -1
	(let* ((midpoint (quotient (+ start stop) 2))
	       (midvalue (vector-ref vec midpoint)))
	  (cond ((< sought midvalue) (loop start (-1+ midpoint)))
		((< midvalue sought) (loop (1+ midpoint) stop))
		(else midpoint))))))

(bsearch v0 7)

