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

(require "private/dataset.rkt")

(provide
 (contract-out

  [load-data-set
   (-> string? symbol? (listof data-set-field?) data-set?)]

  [supported-formats (listof symbol?)]

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
   (-> data-set? exact-positive-integer? (listof string?) data-set?)]

  [partition-for-test
   (-> data-set? (real-in 1.0 50.0) (listof string?) data-set?)]

  [write-snapshot
   (-> data-set? output-port? void?)]

  [read-snapshot
   (-> input-port? data-set?)])

  make-feature

  make-classifier

  data-set?

  data-set-field?)

;; ---------- Requirements

(require "not-implemented.rkt"
         (prefix-in json: "private/json.rkt")
         (prefix-in csv: "private/csv.rkt")
         racket/future
         math/statistics)

;; ---------- Parameters

(define minimum-partition-data-total (make-parameter 100))
(define minimum-partition-data (make-parameter 10))

;; ---------- Implementation

(define (load-data-set name file-format fields)
  (let ([name-set (list->set (for/list ([f fields]) (data-set-field-name f)))])
    (when (not (eq? (length fields) (set-count name-set)))
      (raise-argument-error 'load-data-set "field names must be unique" 2 name format fields)))
  (let ([dataset
         (cond
           [(member file-format json:supported-formats)
            (json:load-data-set name fields)]
           [(member file-format csv:supported-formats)
            (csv:load-data-set name fields)]
           [else (raise-argument-error 'load-data-set (format "one of: ~s" supported-formats) 1 name file-format fields)])])
    (data-set (make-hash
               (for/list ([i (range (length fields))])
                 (cons (data-set-field-name (list-ref fields i)) i)))
              (data-set-features dataset)
              (data-set-classifiers dataset)
              (compute-statistics dataset)
              (data-set-data-count dataset)
              (data-set-partition-count dataset)
              (data-set-partitions dataset))))

(define supported-formats (append json:supported-formats csv:supported-formats))

(define (features ds)
  (data-set-features ds))

(define (classifiers ds)
  (data-set-classifiers ds))

(define (partition-count ds)
  (data-set-partition-count ds))

(define (data-count ds)
  (data-set-data-count ds))

(define (partition ds index)
  (let ([partition-index (partition->index 'partition ds index)])
    (vector-ref (data-set-partitions ds) partition-index)))

(define (feature-vector ds partition-id feature-name)
  (when (not (hash-has-key? (data-set-name-index ds) feature-name))
    (raise-argument-error 'feature-vector (format "one of: ~s" (hash-keys (data-set-name-index ds))) 2 data-set partition feature-name))
  (let* ([partition-index (partition->index 'feature-vector ds partition-id)]
         [feature-index (hash-ref (data-set-name-index ds) feature-name)]
         [a-part (partition ds partition-index)])
    (vector-ref a-part feature-index)))

(define (feature-statistics ds feature-name)
  (when (not (hash-has-key? (data-set-name-index ds) feature-name))
    (raise-argument-error 'feature-vector (format "one of: ~s" (hash-keys (data-set-name-index ds))) 2 data-set partition feature-name))
  (let ([feature-index (hash-ref (data-set-name-index ds) feature-name)])
    (touch (vector-ref (data-set-statistics ds) feature-index))))

(define (classifier-product ds)
  (let* ([names (classifiers ds)]
         [part (partition ds 'default)])
    (classifier-product-strings
          (map (λ (name)
            (vector-ref part (hash-ref (data-set-name-index ds) name)))
            names))))

;; ---------- Implementation (Partitioning)

(define (partition-equally ds p [entropy-classifiers '()])
  (when (not (eq? (partition-count ds) 1))
    (raise-argument-error 'partition-for-test "partition-count /= 1" 1 ds p entropy-classifiers))
  (when (< (data-count ds) (minimum-partition-data-total))
    (raise-argument-error 'partition-for-test (format "data-count < ~a" (minimum-partition-data-total)) 1 ds p entropy-classifiers))
  (when (< (/ (data-count ds) p) (minimum-partition-data))
    (raise-argument-error 'partition-for-test (format "data-count/p < ~a" (minimum-partition-data)) 1 ds p entropy-classifiers))
  (raise-not-implemented 'partition-equally))

(define (partition-for-test ds test-percent [entropy-classifiers '()])
  (when (not (eq? (partition-count ds) 1))
    (raise-argument-error 'partition-for-test "partition-count /= 1" 1 ds test-percent entropy-classifiers))
  (when (< (data-count ds) (minimum-partition-data-total))
    (raise-argument-error 'partition-for-test (format "data-count < ~a" (minimum-partition-data-total)) 1 ds test-percent entropy-classifiers))
  (let ([source (partition ds 'default)]
        [width (hash-count (data-set-name-index ds))]
        [split-at (exact-round (* (data-count ds) (/ test-percent 100)))]
        [partitions (make-vector 2)])
       (vector-set! partitions 0 (make-vector width))
       (vector-set! partitions 1 (make-vector width))
       (for ([feature-idx (range width)])
         (let-values ([(test train) (vector-split-at (vector-ref source feature-idx) split-at)])
           (vector-set! (vector-ref partitions 0) feature-idx train)
           (vector-set! (vector-ref partitions 1) feature-idx test)))
       (data-set (data-set-name-index ds)
                 (data-set-features ds)
                 (data-set-classifiers ds)
                 (data-set-statistics ds)
                 (data-set-data-count ds)
                 (vector-length partitions)
                 partitions)))

;; ---------- Implementation (Snapshots)

(define (write-snapshot ds out)
  (write `(,snapshot-version-number
           ,(data-set-name-index ds)
           ,(data-set-features ds)
           ,(data-set-classifiers ds)
           ,(for/vector ([stat (data-set-statistics ds)]) (when (future? stat) (touch stat)))
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
  (let ([stats-vector (make-vector (length (features ds)))])
    (for ([feature (features ds)])
      (vector-set!
        stats-vector
        (hash-ref (data-set-name-index ds) feature)
        (future (λ () (update-statistics* empty-statistics (feature-vector ds 'default feature))))))
    stats-vector))

(define (partition->index who ds index)
  (cond
    [(number? index)
     (if (>= index (partition-count ds))
       (raise-argument-error who (format "index < ~s" (partition-count ds)) 2 who ds index)
       index)]
    [(eq? index 'default) 0]
    [(eq? index 'training) 0]
    [(eq? index 'testing) 1]
    [else
     (raise-argument-error who "integer or symbol" 2 who ds index)]))

(define (list-unique-strings lst)
  ; (listof string?) -> (listof unique? string?)
  (set->list (for/set ([v lst]) (format "~a" v))))

(define times (string #\⨉))

(define (classifier-product-strings lst)
  ; (listof (vectorof string?)) -> (listof unique? string?)
  (map (lambda (l) (string-join l times))
       (apply cartesian-product (map list-unique-strings lst))))
