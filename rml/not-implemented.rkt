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

(define (exn:fail:not-implemented? e) (exn:fail:unsupported? e))

(define (raise-not-implemented [name #f] [message #f])
  (let ([main-message (cond
                        [(symbol? name)
                         (format "The procedure ~a is not yet implemented." (symbol->string name))]
                        [(string? name)
                         (format "The procedure ~a is not yet implemented." name)]
                        [else "The called procedure is not yet implemented."])])
       (raise (make-exn:fail:unsupported
                (if (string? message) (string-append main-message " Also: "  message) main-message)
                (current-continuation-marks)))))
