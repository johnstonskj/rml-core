#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide iris-data-set small-data-set)

;; ---------- Requirements

(require rml/data)

;; ---------- Implementation

(define iris-data-set
  (load-data-set (path->string (collection-file-path "test/iris_training_data.csv" "rml"))
                 'csv
                 (list
                   (make-feature "sepal-length" #:index 0)
                   (make-feature "sepal-width" #:index 1)
                   (make-feature "petal-length" #:index 2)
                   (make-feature "petal-width" #:index 3)
                   (make-classifier "classification" #:index 4))))

(define small-data-set
  (load-data-set (path->string (collection-file-path "test/simple-test.json" "rml"))
                 'json
                 (list (make-feature "height") (make-classifier "class"))))
