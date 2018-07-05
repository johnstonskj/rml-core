#lang racket/base
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(require rackunit
         math/array
         rml/not-implemented)

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, no parameters"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented))
    "The called procedure is not yet implemented."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with symbol id"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented 'test-case))
    "The procedure test-case is not yet implemented."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with string id"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented "a-test-case"))
    "The procedure a-test-case is not yet implemented."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with other value"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented 99))
    "The called procedure is not yet implemented."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with symbol id and message"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented 'test-case "this is a test."))
    "The procedure test-case is not yet implemented. Also: this is a test."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with string id and message"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented "a-test-case" "this is another test."))
    "The procedure a-test-case is not yet implemented. Also: this is another test."))

(test-case
  "raise-not-implemented: ensure exn:fail:not-implemented caught, with other value and message"
  (check-exn exn:fail:not-implemented?
    (λ () (raise-not-implemented 99 "this is also a test."))
    "The called procedure is not yet implemented. Also: this is also a test."))
