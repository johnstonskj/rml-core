#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         math/array
         rml/not-implemented)

(define (dummy-function x)
  (raise-not-implemented))

  (test-case
    "raise-not-implemented: ensure exn:fail:not-implemented caught"
    (check-exn exn:fail:not-implemented?
      (λ () (dummy-function 'dummy-thing))
      "The called procedure is not yet implemented."))
