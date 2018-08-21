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

(test-case
 "test for documentation completeness"
 (for ([module (list 'rml/classify
                     'rml/data
;                     'rml/gini
                     'rml/individual
                     'rml/not-implemented
                     'rml/results
                     'rml/statistics)])
  (let ([s (open-output-string)])
      (parameterize ([current-error-port s])
        (check-docs module))
        (displayln (get-output-string s))
        (check-eq? (string-length (get-output-string s)) 0))))
