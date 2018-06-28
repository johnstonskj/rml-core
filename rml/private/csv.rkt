#lang racket
;;
;; Racket Machine Learning - Core.
;;
;; ~ Simon Johnston 2018.
;;

(provide
  (contract-out

    [supported-formats (listof symbol?)]

    [load-data-set
     (-> string? (listof data-set-field?) data-set?)]))

;; ---------- Requirements

(require "dataset.rkt"
        csv-reading)

;; ---------- Implementation

(define supported-formats (list 'csv))

(define default-csv-spec '((strip-leading-whitespace? . #t) (strip-trailing-whitespace? . #t)))

(define (load-data-set file-name fields)
  (let* ([file (open-input-file file-name)]
         [reader (make-csv-reader file default-csv-spec)]
         [data (csv->list reader)]
         [rows (length data)]
         [all-names (for/list ([f fields]) (data-set-field-name f))]
         [partition (make-vector (length all-names))])
       (for ([i (length all-names)])
         (vector-set! partition i (make-vector rows)))
       (for ([row rows])
         (let ([rowdata (list-ref data row)])
           (for ([i (length all-names)])
             (let* ([feature (list-ref all-names i)]
                    [field (findf (lambda (f) (eq? (data-set-field-name f) feature)) fields)]
                    [index (data-set-field-index field)]
                    [column (vector-ref partition i)])
               (vector-set! column row
                            (if (data-set-field-numeric? field)
                                (string->number (list-ref rowdata index))
                                (list-ref rowdata index)))))))
       (data-set (make-hash (for/list ([i (length all-names)]) (cons (list-ref all-names i) i)))
                 (map data-set-field-name (filter (λ (f) (data-set-field-feature? f)) fields))
                 (map data-set-field-name (filter (λ (f) (data-set-field-classifier? f)) fields))
                 (make-vector (length all-names))
                 rows
                 1
                 partition)))
