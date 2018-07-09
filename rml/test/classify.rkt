#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/classify
         rml/not-implemented
         rml/results
         "data-sets.rkt")

(define (test-classifier a-data-set an-individual)
  (list (hash-ref an-individual "classification")))

(test-case
  "partition-and-classify: ensure not-implemented"
  (let ([results (partition-and-classify iris-data-set 25.0 test-classifier)])
    (check-eq? (result-value results "Iris-virginica" "Iris-virginica") 45)
    (check-eq? (result-value results "Iris-versicolor" "Iris-versicolor") 45)
    (check-eq? (result-value results "Iris-setosa" "Iris-setosa") 45)))

(test-case
  "cross-train: ensure not-implemented"
  (check-exn exn:fail:not-implemented?
    (λ () (cross-classify iris-data-set 5 test-classifier))))
