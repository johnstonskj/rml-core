#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide data-format^)

;; ---------- Implementation

(define-signature data-format^
  (supported-formats   ; (-> (listof string?))
   load-data-set))     ; ()
