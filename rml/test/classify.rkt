#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/data
         rml/classify
         rml/not-implemented
         rml/results
         "data-sets.rkt")

(define (test-classifier a-data-set against-partition an-individual)
  (list (hash-ref an-individual "classification")))

(test-case
  "partition-and-classify: success with dummy classifier"
  (let ([results (partitioned-test-classify iris-data-set 25.0 test-classifier)]
        [expected (exact-round (* (data-count iris-data-set) 0.25))])
    (check-eq? (result-value results "Iris-virginica" "Iris-virginica") 0)
    (check-eq? (result-value results "Iris-versicolor" "Iris-versicolor") 0)
    (check-eq? (result-value results "Iris-setosa" "Iris-setosa") expected)))

(test-case
  "cross-classify: success with dummy classifier"
  (let ([results (cross-classify iris-data-set 4 test-classifier)]
        [expected (data-count iris-data-set)])
    (check-eq? (result-value results "Iris-virginica" "Iris-virginica") expected)
    (check-eq? (result-value results "Iris-versicolor" "Iris-versicolor") expected)
    (check-eq? (result-value results "Iris-setosa" "Iris-setosa") expected)))
