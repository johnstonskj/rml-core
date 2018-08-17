#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

;; ---------- Requirements

(require rackunit
         racket/string
         math/statistics
         ; ---------
         rml/data
         rml/not-implemented
         rml/statistics
         "data-sets.rkt")

;; ---------- Test Fixtures

;; ---------- Internal procedures

;; ---------- Test Cases

(test-case
 "compute-statistics: success"
 (define stats (compute-statistics iris-data-set))
 (define fstats (feature-statistics stats "sepal-length"))
 (check-true (statistics? fstats)))

(test-case
 "standardize: success"
 (define stats (compute-statistics iris-data-set))
 (define initial-vector (feature-vector iris-data-set default-partition "sepal-length"))
 (check-equal? (vector-length initial-vector) 135)
 (check-equal? (vector-ref initial-vector 0) 5.1)
 (check-equal? (vector-ref initial-vector 1) 4.9)
 (check-equal? (vector-ref initial-vector 134) 5.9)
 (standardize-statistics iris-data-set stats)
 (define standardized-vector (feature-vector iris-data-set default-partition "sepal-length"))
 (check-eq? (vector-length standardized-vector) 135)
 (check-equal? (vector-ref standardized-vector 0) -0.9429540668162042)
 (check-equal? (vector-ref standardized-vector 1) -1.1925595550910812)
 (check-equal? (vector-ref standardized-vector 134) 0.05546788628330822))

