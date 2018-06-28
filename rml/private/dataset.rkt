#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide
 (contract-out

  [make-feature
   (->* (string?) (#:index integer?) data-set-field?)]

  [make-classifier
   (->* (string?) (#:index integer?) data-set-field?)]

  [empty-data-set
   (-> data-set?)])

 (struct-out data-set)

 (struct-out data-set-field))

(struct data-set (
                  name-index
                  features
                  classifiers
                  statistics
                  data-count
                  partition-count
                  partitions))

(define empty-data-set (data-set (hash) '() '() #() 0 0 #()))

(struct data-set-field (
                        name
                        index
                        feature?
                        classifier?
                        numeric?))

(define (make-feature name #:index [index 0])
  (data-set-field name index #t #f #t))

(define (make-classifier name #:index [index 0])
  (data-set-field name index #f #t #f))
