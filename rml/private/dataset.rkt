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

  [empty-data-set data-set?])

  (struct-out data-set)

  (struct-out data-set-field))

;; ---------- Implementation Types

(define-struct/contract data-set-field [
  (name string?)
  (index integer?)
  (feature? boolean?)
  (classifier? boolean?)
  (numeric? boolean?)])

(define-struct/contract data-set [
  (name-index (hash/c string? integer?))
  (features (listof string?))
  (classifiers (listof string?))
  (data-count integer?)
  (partition-count integer?)
  ; partitions : vector -> partition : vector -> feature : vector
  (partitions (vectorof (vectorof (vectorof any/c))))])

;; ---------- Implementation

(define empty-data-set (data-set (hash) '() '() 0 0 #()))

(define (make-feature name #:index [index 0])
  (data-set-field name index #t #f #t))

(define (make-classifier name #:index [index 0])
  (data-set-field name index #f #t #f))
