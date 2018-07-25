#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [classifier/c contract?]

  [classify
   (-> data-set? exact-nonnegative-integer? individual? classifier/c list?)]

  [partitioned-test-classify
   (-> data-set? (real-in 1.0 50.0) classifier/c result-matrix?)]

  [cross-classify
   (-> data-set? exact-positive-integer? classifier/c result-matrix?)]))

;; ---------- Requirements

(require rml/data
         rml/individual
         rml/not-implemented
         rml/results)

;; ---------- Implementation

(define classifier/c
  ; TODO: this should return a hash of classifiers
  (-> data-set? exact-nonnegative-integer? individual? (listof string?)))

(define (classify data-set against-partition individual classifier)
  (classifier data-set against-partition individual))

(define (partitioned-test-classify data-set partition-pc classifier)
  (let ([partitioned (partition-for-test data-set partition-pc '())]
        [results (make-result-matrix data-set)])
    (classify-against partitioned test-partition training-partition classifier results)))

(define (cross-classify data-set partition-count classifier)
  (let ([partitioned (partition-equally data-set partition-count '())]
        [results (make-result-matrix data-set)])
    (for* ([test-idx (range partition-count)]
           [train-idx (range partition-count)])
        (unless (= train-idx test-idx)
          (classify-against partitioned test-idx train-idx classifier results)))
    results))

;; ---------- Internal procedures

(define (classify-against data-set test-partition against-partition classifier results)
  (for ([row (in-producer (individuals data-set test-partition) no-more-individuals)])
    ;; TODO: we need to be able to deal with cross-product hash-refs!
    (record-result results (first (true-w row data-set)) (first (classifier data-set against-partition row))))
  results)

(define (true-w ind data-set)
  (map (λ (c) (hash-ref ind c)) (classifiers data-set)))
