#lang racket
;;
;; Racket Machine Learning - Core.
;;
;;
;; ~ Simon Johnston 2018.

;; ---------- Requirements

(require
  rackunit
  rackunit/docs-complete)

;; ---------- Test Cases

(for ([module '(rml/classify
                rml/data
                ; rml/gini
                rml/individual
                rml/not-implemented
                rml/results
                rml/statistics)])
(test-case
 (format "test for documentation: ~a" module)
 (let ([s (open-output-string)])
   (parameterize ([current-error-port s])
     (check-docs module))
   (define out (get-output-string s))
   (when (non-empty-string? out)
     (displayln out))
   (check-eq? (string-length out) 0))))
