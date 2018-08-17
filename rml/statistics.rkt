#lang racket
;;
;; Racket Machine Learning - Core.
;;
;;
;; ~ Simon Johnston 2018.
;;

(provide
 (contract-out

  [compute-statistics
   (->* (data-set?) ((or/c #f (listof string?))) statistics-hash?)]

  [feature-statistics
   (-> statistics-hash? string? (or/c #f statistics?))]
   
  [standardize-statistics
   (-> data-set? statistics-hash? data-set?)])

 statistics-hash?)

;; ---------- Requirements

(require racket/future
         math/statistics
         rml/data
         rml/not-implemented
         "private/dataset.rkt")

;; ---------- Implementation

(define statistics-hash? (hash/c string? (or/c future? statistics?)))

(define (compute-statistics ds (feature-list #f))
  (define stats-hash (make-hash))
  (for ([feature (if feature-list feature-list (features ds))])
    (hash-set!
     stats-hash
     feature
     (future (λ () (for/fold
                    ([running-stats empty-statistics])
                    ([part (partition-count ds)])
                     (update-statistics*
                      running-stats
                      (feature-vector ds part feature)))))))
  stats-hash)

(define (feature-statistics stats-hash feature)
  (define stats (hash-ref stats-hash feature #f))
  (if (future? stats)
      (touch stats)
      stats))

(define (standardize-statistics ds stats-hash)
  (for ([feature (hash-keys stats-hash)])
    (define stats (feature-statistics stats-hash feature))
    (for ([part (partition-count ds)])
      (define new-vector (vector-standardize
                          (feature-vector ds part feature)
                          (statistics-mean stats)
                          (statistics-stddev stats)))
      (let ([the-partition (partition ds part)]
            [feature-index (hash-ref (data-set-name-index ds) feature)])
        (vector-set! the-partition feature-index new-vector))))
  ds)

;; ---------- Internal procedures

(define/contract
  (vector-standardize v μ σ)
  (-> (vectorof number?) inexact? inexact? (vectorof number?))
  (for/vector ([e v])
    (/ (- e μ) σ)))

