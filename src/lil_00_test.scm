;;
;; lil_00_test.scm - schemer tests
;;

(load "test-manager/load.scm")
(load "lil_00.scm")

(in-test-group
 schemer

 ;; null?
 (define-test (null-tests)
   "Checking that 'null?' works"
   (check (null? '()) "Empty list is null")
   (check (not (null? (list 'a 'b))) "Non-empty list is not null"))

 (define-test (atom-tests)
   "Checking that 'atom?' works"
   (check (atom? 'fred) "String 'fred' is an atom")
   (check (atom? 43) "Number '43' is an atom")
   (check (not (atom? '())) "Empty list is not an atom")
   (check (not (atom? (list 43 67))) "List '(43 67)' is not an atom"))

 (define-test (lat-tests)
   "Checking that 'lat?' works"
   (check (lat? '()) "True for empty list")
   (check (lat? (list 'fred 'wilma)) "List '(fred wilma)' is true")
   (check (not (lat? (list 'fred (list 'wilma 'barney)))) "List '(fred (wilma barney))' is false"))

 (define-test (member-rember-tests)
   "Checking that 'member?' and 'rember' works"
   (check (member? 'c '(x y a b c y)))
   (check (not (member? 'g '(x y a b c y))))
   (check (list-equal? (rember 'z '(x y z a)) '(x y a)))
   (check (list-equal? (rember 'g '(x y z a)) '(x y z a))))

 (define-test (list-equal-tests)
   "Checking if 'list-equal?' works"
   (check (list-equal? '() '()))
   (check (list-equal? '(a b c) '(a b c)))
   (check (not (list-equal? '(b) '(a)))))
 
 (run-registered-tests))

