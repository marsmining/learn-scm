;;
;; sicp3-3.scm
;;

;; 3.3.2 - queues
;;

(define (make-queue) (cons ’() ’()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define q (make-queue))

(empty-queue? q)
