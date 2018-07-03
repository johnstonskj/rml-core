#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/data
         rml/results)

(define iris-data-set
  (load-data-set "./iris_training_data.csv"
                 'csv
                 (list
                   (make-feature "sepal-length" #:index 0)
                   (make-feature "sepal-width" #:index 1)
                   (make-feature "petal-length" #:index 2)
                   (make-feature "petal-width" #:index 3)
                   (make-classifier "classification" #:index 4))))


(test-case
  "result-matrix?: true"
  (check-true (result-matrix? (make-result-matrix iris-data-set))))

(test-case
  "result-matrix?: false"
  (check-false (result-matrix? "not an individual")))

(test-case
  "make-result-matrix: success"
  (let ([results (make-result-matrix iris-data-set)])
    (check-equal? 0 (result-value results "Iris-versicolor" "Iris-versicolor"))))

(test-case
  "result-value: bad classifiers"
  (let ([results (make-result-matrix iris-data-set)])
    (check-exn exn:fail:contract?
      (λ () (result-value results "Iris-versicolor" "Not An Iris")))))

;; TODO: tests for result-matrix-formatted
