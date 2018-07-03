#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         racket/contract
         (except-in racket/list partition)
         rml/data
         rml/results)

(define iris-data-set
  (load-data-set (path->string (collection-file-path "test/iris_training_data.csv" "rml"))
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
       (check-equal? (result-value results "Iris-versicolor" "Iris-versicolor") 0)))

(test-case
  "result-value: bad classifiers"
  (let ([results (make-result-matrix iris-data-set)])
       (check-exn exn:fail:contract?
         (λ () (result-value results "Iris-versicolor" "Not An Iris")))))

(test-case
  "record-result: success"
  (let ([results (make-result-matrix iris-data-set)])
       (check-equal? (result-value results "Iris-versicolor" "Iris-versicolor") 0)
       (record-result results "Iris-versicolor" "Iris-versicolor")
       (check-equal? (result-value results "Iris-versicolor" "Iris-versicolor") 1)
       (record-result results "Iris-versicolor" "Iris-versicolor")
       (check-equal? (result-value results "Iris-versicolor" "Iris-versicolor") 2)
       (record-result results "Iris-versicolor" "Iris-versicolor")
       (check-equal? (result-value results "Iris-versicolor" "Iris-versicolor") 3)))

(test-case
  "result-matrix-formatted: success"
  (let* ([results (make-result-matrix iris-data-set)]
         [printable (result-matrix-formatted results)]
         [xy (add1 (length (classifier-product iris-data-set)))])
        (check-eq? (first (first printable)) "true ω pred")
        (check-eq? (length printable) xy)
        (check-eq? (length (first printable)) xy)))
