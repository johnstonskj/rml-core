#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide
  (contract-out

    [supported-formats
     (-> (listof string?))]

    [load-data-set
     (-> string? (listof data-set-field?) data-set?)]))

;; ---------- Requirements

(require "dataset.rkt"
        json)

;; ---------- Implementation

(define supported-formats '('json))

(define (load-data-set file-name fields)
  (let* ([file (open-input-file file-name)]
         [data (read-json file)]
         [rows (length data)]
         [all-names (for/list ([f fields]) (data-set-field-name f))]
         [partition (make-vector (length all-names))])
    (for ([i (length all-names)])
      (vector-set! partition i (make-vector rows)))
    (for ([row rows])
      (let ([rowdata (list-ref data row)])
        (for ([i (length all-names)])
          (let ([feature (list-ref all-names i)]
                [column (vector-ref partition i)])
            (vector-set! column row (hash-ref rowdata (string->symbol feature)))))))
    (data-set (make-hash (for/list ([i (length all-names)]) (cons (list-ref all-names i) i)))
              (map data-set-field-name (filter (lambda (f) (data-set-field-feature? f)) fields))
              (map data-set-field-name (filter (lambda (f) (data-set-field-classifier? f)) fields))
              (make-vector (length all-names))
              rows
              1
              partition)))
