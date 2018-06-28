#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; A simple structure for managing data sets.
;;
;; Feature transformation details:
;;   http://www.scholarpedia.org/article/K-nearest_neighbor
;;
;; ~ Simon Johnston 2018.
;;

(define snapshot-version-number 1.0)

(provide
 (contract-out

  [load-data-set
   (-> string? symbol? (listof data-set-field?) data-set?)]

  [data-set?
   (-> any/c boolean?)]

  [features
   (-> data-set? (listof string?))]

  [classifiers
   (-> data-set? (listof string?))]

  [partition-count
   (-> data-set? exact-nonnegative-integer?)]

  [data-count
   (-> data-set? exact-nonnegative-integer?)]

  [partition
   (-> data-set? (or/c exact-nonnegative-integer? symbol?) (vectorof vector?))]

  [feature-vector
   (-> data-set? (or/c exact-nonnegative-integer? symbol?) string? vector?)]

  [feature-statistics
   (-> data-set? string? statistics?)]

  [classifier-product
   (-> data-set? (listof string?))]

  [partition-equally
   (-> data-set? exact-positive-integer? (listof string?))]

  [partition-for-test
   (-> data-set? (real-in 1.0 50.0) (listof string?) data-set?)]

  [standardize
   (-> data-set? (non-empty-listof string?) data-set?)]

  [fuzzify
   (-> data-set? (non-empty-listof string?) data-set?)]

  [write-snapshot
   (-> data-set? output-port? void?)]

  [read-snapshot
   (-> input-port? data-set?)]))

;; ---------- Requirements

(require "notimplemented.rkt"
         "dataset.rkt"
         (prefix-in json: "json.rkt")
         (prefix-in csv: "csv.rkt")
         racket/future
         math/statistics)

;; ---------- Implementation

(define all-formats (append json:supported-formats csv:supported-formats))

(define (load-data-set name format fields)
  (let ([name-set (list->set (for/list ([f fields]) (data-set-field-name f)))])
    (when (not (eq? (length fields) (set-count name-set)))
      (raise-argument-error 'load-data-set "field names must be unique" 2 name format fields)))
  (let ([dataset
         (cond
           [(member format json:supprted-formats)
            (json:load-data-set name fields)]
           [(member format csv:supprted-formats)
            (csv:load-data-set name fields)]
           [else (raise-argument-error 'load-data-set (format "one of: ~s" (all-formats)) 1 name format fields)])])
    (data-set (make-hash
               (for/list ([i (range (length fields))])
                 (cons (data-set-field-name (list-ref fields i)) i)))
              (data-set-features dataset)
              (data-set-classifiers dataset)
              (compute-statistics dataset)
              (data-set-data-count dataset)
              (data-set-partition-count dataset)
              (data-set-partitions dataset))))

(define (features ds)
  (data-set-features ds))

(define (classifiers ds)
  (data-set-classifiers ds))

(define (partition-count ds)
  (data-set-partition-count ds))

(define (data-count ds)
  (data-set-data-count ds))

(define (partition ds index)
  (let ([partition-index (partition->index 'partition ds partition)])
    (data-set-partitions ds))) ; TODO: vector of vectors

(define (feature-vector ds partition feature-name)
  (when (not (hash-has-key? (data-set-name-index ds) feature-name))
    (raise-argument-error 'feature-vector (format "one of: ~s" (hash-keys (data-set-name-index ds))) 2 data-set partition feature-name))
  (let ([partition-index (partition->index 'feature-vector ds partition)]
        [feature-index (hash-ref (data-set-name-index ds) feature-name)]
        [partition (data-set-partitions ds)])
    (vector-ref partition feature-index)))

(define (feature-statistics ds feature-name)
  (when (not (hash-has-key? (data-set-name-index ds) feature-name))
    (raise-argument-error 'feature-vector (format "one of: ~s" (hash-keys (data-set-name-index ds))) 2 data-set partition feature-name))
  (let ([feature-index (hash-ref (data-set-name-index ds) feature-name)])
    (touch (vector-ref (data-set-statistics ds) feature-index))))

(define (classifier-product ds)
  (let* ([names (classifiers ds)]
         [partition (data-set-partitions ds)])
    (classifier-product-strings
     (map (lambda (name) (vector-ref partition (hash-ref (data-set-name-index ds) name))) names))))

;; ---------- Implementation (Partitioning)

(define (partition-equally ds k [entropy-classifiers '()])
  (raise-not-implemented))

(define (partition-for-test ds test-percent [entropy-classifiers '()])
  (raise-not-implemented))

;; ---------- Implementation (Feature Transformation)

(define (standardize data-set features)
  ; z_{ij} = x_{ij}-μ_j / σ_j
  (raise-not-implemented))

(define (fuzzify data-set features)
  (raise-not-implemented))

;; ---------- Implementation (Snapshots)

(define (write-snapshot ds out)
  (write `(,snapshot-version-number
           ,(data-set-name-index ds)
           ,(data-set-features ds)
           ,(data-set-classifiers ds)
           ,(for/list ([stat (data-set-statistics ds)]) (when (future? stat) (touch stat)))
           ,(data-set-data-count ds)
           ,(data-set-partition-count ds)
           ,(data-set-partitions ds))
         out))

(define (read-snapshot in)
  (let* ([values (read in)]
         [version (first values)])
    ; TODO: check for version mismatch
    (apply data-set (rest values))))

;; ---------- Internal procedures

(define (compute-statistics ds)
  (for/list ([feature (data-set-features ds)])
    (vector-set!
     (data-set-statistics ds)
     (hash-ref (data-set-name-index ds) feature)
     (future (lambda () (update-statistics* empty-statistics (feature-vector ds 'default feature))))))
  (data-set-statistics ds))

(define (partition->index who ds partition)
  (cond
    [(number? partition)
     (when (>= partition (data-set-partition-count ds))
       (raise-argument-error who (format "< ~s" (data-set-partition-count ds)) 1 who ds partition))]
    [(eq? partition 'default) 0]
    [(eq? partition 'training) 0]
    [(eq? partition 'testing) 1]
    [else
     (raise-argument-error who (format "< ~s" (data-set-partition-count ds)) 1 who ds partition)]))

(define (list-unique-strings lst)
  (set->list (for/set ([v lst]) (format "~a" v))))

(define times (string #\⨉))

(define (classifier-product-strings lst)
  (map (lambda (l) (string-join l times))
       (apply cartesian-product (map list-unique-strings lst))))
