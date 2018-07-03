#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/data)

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
  "supported-formats: includes core formats"
  (check-not-false (member 'csv supported-formats))
  (check-not-false (member 'json supported-formats)))

(test-case
  "load-data-set: successful load (json)"
  (let* ([fields (list (make-feature "height") (make-classifier "class"))]
         [dataset (load-data-set (path->string (collection-file-path "test/simple-test.json" "rml")) 'json fields)])
    (check-eq? 1 (length (features dataset)))
    (check-eq? 1 (length (classifiers dataset)))
    (check-eq? 2 (length (classifier-product dataset)))
    (check-eq? 1 (partition-count dataset))
    (check-eq? 7 (data-count dataset))))

(test-case
  "load-data-set: successful load (csv)"
  (check-eq? 4 (length (features iris-data-set)))
  (check-eq? 1 (length (classifiers iris-data-set)))
  (check-eq? 3 (length (classifier-product iris-data-set)))
  (check-eq? 1 (partition-count iris-data-set))
  (check-eq? 135 (data-count iris-data-set)))
