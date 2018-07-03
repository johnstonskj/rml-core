#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         rml/data
         rml/not-implemented
         math/statistics)

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
        (check-eq? (length (features dataset)) 1)
        (check-eq? (length (classifiers dataset)) 1)
        (check-eq? (length (classifier-product dataset)) 2)
        (check-eq? (partition-count dataset) 1)
        (check-eq? (data-count dataset) 7)))

(test-case
  "load-data-set: successful load (csv)"
  (check-eq? (length (features iris-data-set)) 4)
  (check-eq? (length (classifiers iris-data-set)) 1)
  (check-eq? (length (classifier-product iris-data-set)) 3)
  (check-equal? (sort '("sepal-length" "sepal-width" "petal-length" "petal-width" "classification") string<?)
                (sort (append (features iris-data-set) (classifiers iris-data-set)) string<?))
  (check-eq? (partition-count iris-data-set) 1)
  (check-eq? (data-count iris-data-set) 135))

(test-case
  "load-data-set: fail with duplicate names"
  (let ([fields (list (make-feature "height") (make-feature "class") (make-classifier "class"))])
       (check-exn exn:fail:contract?
         (λ () (load-data-set (path->string (collection-file-path "test/data.rkt" "rml")) 'racket fields)))))

(test-case
  "load-data-set: fail with bad format"
  (let ([fields (list (make-feature "height") (make-classifier "class"))])
       (check-exn exn:fail:contract?
         (λ () (load-data-set (path->string (collection-file-path "test/data.rkt" "rml")) 'racket fields)))))

(test-case
  "load-data-set: fail with bad file name"
  (let ([fields (list (make-feature "height") (make-classifier "class"))])
       (check-exn exn:fail:filesystem:errno?
         (λ () (load-data-set (path->string (collection-file-path "test/simple-fail.json" "rml")) 'json fields)))))

(test-case
  "partition: test access"
  (check-eq? (vector-length (partition iris-data-set 0)) 5)
  (check-eq? (vector-length (partition iris-data-set 'default)) 5)
  (check-exn exn:fail:contract?
    (λ () (partition iris-data-set 99)))
  (check-exn exn:fail:contract?
    (λ () (partition iris-data-set 'unknown))))

(test-case
  "feature-vector: success"
  (let ([fvector (feature-vector iris-data-set 'default "sepal-length")])
       (check-true (vector? fvector))
       (check-eq? (vector-length fvector) 135)))

(test-case
  "feature-vector: success"
  (let ([fstats (feature-statistics iris-data-set "sepal-length")])
       (check-true (statistics? fstats))))

; (test-case
;   "write-snapshot: success"
;   (let* ([fields (list (make-feature "height") (make-classifier "class"))]
;          [dataset (load-data-set (path->string (collection-file-path "test/simple-test.json" "rml")) 'json fields)]
;          [out (open-output-string)])
;     (write-snapshot dataset out)
;     (let ([newdata (read-snapshot (open-input-string (get-output-string out)))])
;       (check-eq? 1 (length (features dataset)))
;       (check-eq? 1 (length (classifiers dataset)))
;       (check-eq? 2 (length (classifier-product dataset)))
;       (check-eq? 1 (partition-count dataset))
;       (check-eq? 7 (data-count dataset)))))

(test-case
  "partition-and-classify: ensure not-implemented"
  (check-exn exn:fail:not-implemented?
    (λ () (partition-equally iris-data-set 5 '()))))

(test-case
  "partition-for-test: ensure not-implemented"
  (check-exn exn:fail:not-implemented?
    (λ () (partition-for-test iris-data-set 25.0 '()))))
