#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [classify
   (-> data-set? classifier/c list?)]

  [test-classify
   (-> data-set? (real-in 1.0 50.0) classifier/c result-matrix?)]

  [cross-classify
   (-> data-set? exact-positive-integer? classifier/c result-matrix?)])

  classifier/c)

;; ---------- Requirements

(require rml/data
         rml/individual
         rml/not-implemented
         rml/results)

;; ---------- Implementation

(define classifier/c
  ; TODO: this should return a hash of classifiers
  (-> data-set? individual? (listof string?)))

(define (classify data-set classifier)
  (raise-not-implemented 'cross-train))

(define (test-classify data-set partition-pc classifier)
  (let* ([partitioned (partition-for-test data-set partition-pc '())]
         [training (partition partitioned 'training)]
         [testing (partition partitioned 'testing)]
         [results (make-result-matrix data-set)])
    (for ([row (in-producer (individuals data-set 0) no-more-individuals)])
      ;; TODO: we need to be able to deal with cross-product hash-refs!
      (record-result results (first (true-w row data-set)) (first (classifier data-set row))))
    results))

(define (cross-classify data-set partition-count classifier)
  (let ([partitioned (partition-equally data-set partition-count '())])
    (displayln partitioned))
  (raise-not-implemented 'cross-train))

;; ---------- Internal procedures

(define (true-w ind data-set)
  (map (λ (c) (hash-ref ind c)) (classifiers data-set)))
