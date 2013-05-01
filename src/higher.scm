;;
;; higher.scm
;;

;; fold left - uses an accumulator loop, where we keep
;; track of 'result' which starts at 'initial' and 'rest'
;; which starts at 'sequence'. Each iteration, 'result'
;; becomes 'op result head' and 'rest' becomes 'tail'.
;;
;; (op initial head)
;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;; fold right - calls the 'op' operator with args 'head'
;; and recursive call to 'fold-right' with 'tail'.
;;
;; (op head initial)
;;
;; eg: call 'fold-right' 'fr'..
;;   (fr + 0 (list 1 2 3))
;;   (+ head (fr + 0 (2 3)))
;;   (+ 1 (+ 2 (fr + 0 (3))))
;;   (+ 1 (+ 2 (+ 3 (fr + 0 ()))))
;;   (+ 1 (+ 2 (+ 3 0)))
;;
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))


(define nil '()) 

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(fold-right + 0 (list 1 2 3))
(+ 1 (+ 2 (+ 3 0)))

(define (append xs ys)
  (if (null? xs) ys
      (cons (car xs) (append (cdr xs) ys))))

(append (list 1 2 3) (list 4 5 6))

(define (reverse sequence)
  (fold-right (lambda (x xs) (append xs (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (xs x) (cons x xs)) nil sequence))

(reverse (list 1 2 3))

