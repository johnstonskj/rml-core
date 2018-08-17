#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide
  (contract-out

    [make-individual
     (->* () (#:data-set data-set?) #:rest any/c individual?)]

    [data-set-individual
     (-> data-set? individual?)]

    [individual?
     (-> any/c boolean?)]

    [no-more-individuals symbol?]

    [individuals
     (-> data-set? exact-nonnegative-integer? generator?)]))

;; ---------- Requirements

(require "data.rkt"
         "private/dataset.rkt"
         racket/generator)

;; ---------- Implementation

(define (individual? a)
  (if (hash? a)
      (empty? (filter-not string? (hash-keys a)))
      #f))

(define (make-individual #:data-set [dataset #f] . lst)
  (let ([ind (apply hash lst)])
    (when (not (individual? ind))
        (raise-argument-error 'make-individual "not a valid hash construction list" 0 lst))
    (when dataset
      (let ([ds-names (list->set (append (features dataset) (classifiers dataset)))]
            [in-names (list->set (hash-keys ind))])
        (when (not (set=? ds-names in-names))
          (raise-argument-error 'make-individual "construction list does not match data-set" 0 lst))))
    ind))

(define (data-set-individual dataset)
  (make-hash (hash-map (data-set-name-index dataset) (λ (k v) (cons k #f)))))

(define no-more-individuals (gensym))

(define (individuals ds partition-id)
 (generator ()
   (let ([source (partition ds partition-id)])
        (for ([row (range (vector-length (vector-ref source 0)))])
             (yield (make-hash
               (hash-map (data-set-name-index ds)
                         (λ (k v)
                           (cons k (vector-ref
                                    (vector-ref source (hash-ref (data-set-name-index ds) k))
                                    row)))))))
        no-more-individuals)))
