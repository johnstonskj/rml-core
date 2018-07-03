#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; Implements a simple macro for raising a correctly formatted error reporting
;; the lack of implementation for a given procedure. This is significantly
;; better than returning dummy results while developing modules.
;;
;; ~ Simon Johnston 2018.
;;

(provide
  
  exn:fail:not-implemented?

  raise-not-implemented)

;; ---------- Implementation

(define-syntax (raise-not-implemented stx)
  (syntax-case stx ()
    [(expr)
      (quasisyntax/loc stx
        (raise (make-exn:fail:not-implemented
                  "The called procedure is not yet implemented."
                  (current-continuation-marks)
                  (srcloc '#,(syntax-source #'expr)
                          '#,(syntax-line #'expr)
                          '#,(syntax-column #'expr)
                          '#,(syntax-position #'expr)
                          '#,(syntax-span #'expr)))))]))

;; ---------- Internal types

(define-struct (exn:fail:not-implemented
                exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:not-implemented
         (msg marks a-srcloc))
       (list a-srcloc)])))
