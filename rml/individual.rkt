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
     (-> any/c boolean?)]))

;; ---------- Requirements

(require "data.rkt")

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
      (let ([ds-names (set (append (features dataset) (classifiers dataset)))]
            [in-names (set (hash-keys ind))])
        (when (not (set-equal? ds-names in-names))
          (raise-argument-error 'make-individual "construction list does not match data-set" 0 lst))))
    ind))

(define (data-set-individual dataset)
  (make-hash (append (for/list ([name (features dataset)]) (cons name #f))
                     (for/list ([name (classifiers dataset)]) (cons name #f)))))
