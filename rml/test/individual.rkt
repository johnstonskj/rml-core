#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/data
         rml/individual)

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
  "individual?: true"
  (check-true (individual? (make-individual "height" 199 "class" "m"))))

(test-case
  "individual?: pseudo-true"
  (check-true (individual? (hash "height" 199 "class" "m"))))

(test-case
  "individual?: false"
  (check-false (individual? "not an individual"))
  (check-false (individual? (hash "height" 199 'class "m"))))

(test-case
  "make-individual: hash creation success"
  (let ([individual (make-individual "height" 199 "class" "m")])
        (check-eq? 2 (hash-count individual))
        (check-equal? (sort '("height" "class") string<?) (sort (hash-keys individual) string<?))
        (check-eq? 199 (hash-ref individual "height"))
        (check-eq? "m" (hash-ref individual "class"))))

(test-case
  "make-individual: fail on odd parameters"
  (check-exn exn:fail:contract?
    (λ () (make-individual "height" 199 "class"))))

(test-case
  "make-individual: fail on non-string key"
  (check-exn exn:fail:contract?
    (λ () (make-individual "height" 199 'class "m"))))

(test-case
  "make-individual: hash with data-set creation success"
  (let ([individual (make-individual "sepal-length" 6.3
                                     "sepal-width" 2.5
                                     "petal-length" 4.9
                                     "petal-width" 1.5
                                     "classification" "Iris-versicolor"
                                     #:data-set iris-data-set)])
        (check-eq? 5 (hash-count individual))
        (check-equal?
          (sort '("sepal-length" "sepal-width" "petal-length" "petal-width" "classification") string<?)
          (sort (hash-keys individual) string<?))
        (check-eq? 6.3 (hash-ref individual "sepal-length"))
        (check-eq? "Iris-versicolor" (hash-ref individual "classification"))))

(test-case
  "make-individual: hash with data-set fail on mismatch names"
  (check-exn exn:fail:contract?
    (λ () (make-individual "sepal-length" 6.3
                           "sepal-wide" 2.5 ; wide /= width
                           "petal-length" 4.9
                           "petal-width" 1.5
                           "classification" "Iris-versicolor"
                           #:data-set iris-data-set))))

(test-case
  "make-individual: hash with data-set fail with too few names"
  (check-exn exn:fail:contract?
    (λ () (make-individual "sepal-length" 6.3
                           "petal-length" 4.9
                           "petal-width" 1.5
                           "classification" "Iris-versicolor"
                           #:data-set iris-data-set))))

(test-case
  "make-individual: hash with data-set fail with too many names"
  (check-exn exn:fail:contract?
    (λ () (make-individual "sepal-length" 6.3
                           "sepal-width" 2.5
                           "petal-length" 4.9
                           "petal-width" 1.5
                           "petal-color" 254
                           "classification" "Iris-versicolor"
                           #:data-set iris-data-set))))

(test-case
  "data-set-individual: creation success"
  (let ([individual (data-set-individual iris-data-set)])
       (check-eq? 5 (hash-count individual))
       (check-equal?
         (sort '("sepal-length" "sepal-width" "petal-length" "petal-width" "classification") string<?)
         (sort (hash-keys individual) string<?))
       (check-eq? #f (hash-ref individual "sepal-length"))
       (check-eq? #f (hash-ref individual "classification"))))
