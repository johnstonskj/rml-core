#lang info
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(define collection 'multi)

(define pkg-desc "Racket Machine Learning - Core")
(define version "1.0")
(define pkg-authors '(johnstonskj))

(define deps '(
  "base"
  "math-lib"
  "csv-reading"
  "mcfly"          ;; mcfly required by csv-reading
  "rackunit-lib"
  "racket-index"))
(define build-deps '(
  "scribble-lib"
  "racket-doc"
  "math-doc"
  "sandbox-lib"
  "cover-coveralls"))
