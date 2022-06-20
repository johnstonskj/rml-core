#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

;; ---------- Requirements

(require rackunit
         ; ---------
         rml/gini)

;; ---------- Test Fixtures

(define XY-data
  (list
   (hash "X1" 2.771244718 "X2" 1.784783929 "Y" 0)
   (hash "X1" 1.728571309 "X2" 1.169761413 "Y" 0)
   (hash "X1" 3.678319846 "X2" 2.81281357 "Y" 0)
   (hash "X1" 3.961043357 "X2" 2.61995032 "Y" 0)
   (hash "X1" 2.999208922 "X2" 2.209014212 "Y" 0)
   (hash "X1" 7.497545867 "X2" 3.162953546 "Y" 1)
   (hash "X1" 9.00220326 "X2" 3.339047188 "Y" 1)
   (hash "X1" 7.444542326 "X2" 0.476683375 "Y" 1)
   (hash "X1" 10.12493903 "X2" 3.234550982 "Y" 1)
   (hash "X1" 6.642287351 "X2" 3.319983761 "Y" 1)))

(define sample-1
  (list
   (hash
    "data" 2
    "class" "1")
   (hash
    "data" 4
    "class" "1")))

(define sample-2
  (list
   (hash
    "data" 6
    "class" "0")
   (hash
    "data" 8
    "class" "0")))

(define samplesets
  (list sample-1 sample-2))

(define total-size (+ (length sample-1) (length sample-2)))

(define bin-hash (make-hash (list (cons "0" 0) (cons "1" 0))))

;; ---------- Internal procedures

;; ---------- Test Cases

(test-case
 "test sample-partition-individuals success"
 (let-values ([(left right) (sample-partition-individuals XY-data "X1" 6)])
   (check-eq? (length left) 5)
   (check-eq? (length right) 5)))

(test-case
 "test gini-find-optimal-value-c success"
 (let-values ([(value score) (gini-find-optimal-value-c XY-data "X1" "Y" '(0 1))])
   (check-eq? value 6.642287351)
   (check-eq? score 0.0)))

(test-case
 "test gini-find-optimal-c success"
 (let-values ([(feature value score) (gini-find-optimal-c XY-data '("X1" "X2") "Y" '(0 1))])
   (check-eq? feature "X1")
   (check-eq? value 6.642287351)
   (check-eq? score 0.0)))

(test-case
 "test gini-score success"
 (let ([score (gini-score samplesets "class" '("0" "1") 4)])
   (check-eq? score 0.0)))
