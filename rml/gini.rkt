#lang racket
;;
;; Racket Machine Learning - Core.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [gini-find-optimal
   (->* (data-set?)
        (partition-id? boolean? (or/c #f (-> any/c boolean?)))
        (values string? inexact? inexact?))]
  
  [gini-find-optimal-c
   (->* (list? (listof string?) string? list?)
        (boolean? (or/c #f (-> any/c boolean?)))
        (values string? inexact? inexact?))]
  
  [gini-find-optimal-value
   (->* (data-set? string?)
        (partition-id? (or/c #f (-> any/c boolean?)))
        (values inexact? inexact?))]

  [gini-find-optimal-value-c
   (->* (list? string? string? list?)
        ((or/c #f (-> any/c boolean?)))
        (values inexact? inexact?))]

  [gini-score
   (-> list? string? list? exact? inexact?)]
  
  [gini-perfect-score inexact?]

  [sample-individuals
   (->* (list?) (exact-nonnegative-integer? exact-nonnegative-integer?) list?)]

  [sample-partition-individuals
   (->* (list? string? any/c)
        ((-> any/c boolean?))
        (values list? list?))]

  [sample-random-features
   (-> (listof string?) (listof string?))]))

;; ---------- Requirements

(require racket/random
         (rename-in rml/data
                    (partition dataset-partition)
                    (features dataset-features)
                    (classifiers dataset-classifiers)))

;; ---------- Internal types

;; ---------- Implementation

(define (gini-find-optimal dataset (partition default-partition) (sample-features #f) (predicate #f))
  (gini-find-optimal-c
   dataset ;sample
   (dataset-features dataset)
   (first (dataset-classifiers dataset))
   (classifier-product dataset)
   predicate))

(define (gini-find-optimal-c
         sample
         features
         classifier-key
         classifier-values
         (sample-features #f)
         (predicate #f))
  (let ([min-gini 999] [min-feature ""] [min-value 0])
    (for ([feature (if sample-features (sample-random-features features) features)])
      (let-values ([(a-min-value a-min-gini)
                    (gini-find-optimal-value-c sample
                                               feature
                                               classifier-key
                                               classifier-values
                                               predicate)])
        (when (< a-min-gini min-gini)
          (set! min-gini a-min-gini)
          (set! min-feature feature)
          (set! min-value a-min-value))))
    (values min-feature min-value min-gini)))

(define (gini-find-optimal-value dataset feature (partition default-partition) (predicate #f))
  #f)

(define (gini-find-optimal-value-c sample feature classifier-key classifier-values (predicate #f))
  (let ([min-gini 999] [min-value 0])
    (for ([individual sample])
      (let-values ([(left right)
                    (sample-partition-individuals sample
                                                  feature
                                                  (hash-ref individual feature)
                                                  predicate)])
        (let ([gini (exact->inexact
                     (gini-score (list left right)
                                 classifier-key classifier-values
                                 (length sample)))])
          (when (< gini min-gini)
            (set! min-gini gini)
            (set! min-value (hash-ref individual feature))))))
    (values min-value min-gini)))

(define gini-perfect-score 0.0)

(define (gini-score samplesets classifier-key classifier-values total-size)
  (let ([zeroed-hash (make-hash (map (λ (c) (cons c 0)) classifier-values))])
    (apply + (for/list ([sample samplesets])
               (let* ([sample-weight (/ (length sample) total-size)]
                      [sample-hash (sample-individuals->hash sample
                                                             (hash-copy zeroed-hash)
                                                             classifier-key)]
                      [proportions (sample-proportions sample-hash)]
                      [sum (apply + (map (curryr expt 2) (hash-values proportions)))])
                 (exact->inexact (* (- 1 sum) sample-weight)))))))

(define (sample-partition-individuals sample feature value (predicate #f))
  (partition
   (λ (i)
     (if predicate (predicate (hash-ref i feature)) (< (hash-ref i feature) value)))
   sample))

(define (sample-individuals sample (low-pc 20) (high-pc 80))
  (random-sample sample (random low-pc high-pc)))

(define minimum-feature-count (make-parameter 1))

(define (sample-random-features features)
  (let* ([raw-feature-count (inexact->exact (sqrt (length features)))]
         [feature-count (if (> raw-feature-count 0) raw-feature-count (minimum-feature-count))])
    (random-sample features feature-count)))

;; ---------- Internal procedures

(define/contract
  (sample-individuals->hash individuals bin-hash key)
  (-> list? hash? string? hash?)
  ;; like sample->hash but specifically for individuals
  (for ([individual individuals])
    (let ([classifier (hash-ref individual key)])
      (hash-update! bin-hash classifier add1)))
  bin-hash)

(define/contract
  (sample-proportions bin-hash)
  (-> hash? hash?)
  ;; return a hash where the result of sample->hash values are
  ;; turned into proportions of each vs. total samples
  (let ([total-samples (apply + (hash-values bin-hash))])
    (make-hash (hash-map bin-hash (λ (classifier count)
                                    (cons classifier
                                          (if (= total-samples 0) 0 (/ count total-samples))))))))
