#lang racket
;;
;; Racket Machine Learning - K-NN.
;;
;; ~ Simon Johnston 2018.


(provide
 (contract-out

  [result-matrix?
   (-> any/c boolean?)]

  [make-result-matrix
   (-> data-set? result-matrix?)]

  [record-result
   (-> result-matrix? any/c any/c result-matrix?)]

 [result-value
  (-> result-matrix? any/c any/c integer?)]

  [result-matrix-formatted
   (-> result-matrix? list?)]))

;; ---------- Requirements

(require rml/data)

;; ---------- Internal types

(struct confusion-matrix
 (values
  Ω
  results))

;; ---------- Implementation

(define (result-matrix? a)
  (confusion-matrix? a))

(define (make-result-matrix data-set)
  (let* ([values (classifier-product data-set)]
         [Ω (length values)])
    (confusion-matrix
     (make-hash (for/list ([i (length values)]) (cons (list-ref values i) i)))
     Ω
     (apply vector (for/list ([i (range Ω)]) (make-vector Ω))))))

(define (record-result C true-ω predicted-ω)
  (let ([true-i (hash-ref (confusion-matrix-values C) true-ω)]
        [predicted-i (hash-ref (confusion-matrix-values C) predicted-ω)])
    (vector-set! (vector-ref (confusion-matrix-results C) true-i)
                 predicted-i
                 (add1 (vector-ref (vector-ref (confusion-matrix-results C) true-i) predicted-i))))
  C)

(define (result-value C true-ω predicted-ω)
  (let ([true-i (hash-ref (confusion-matrix-values C) true-ω)]
        [predicted-i (hash-ref (confusion-matrix-values C) predicted-ω)])
        (vector-ref (vector-ref (confusion-matrix-results C) true-i) predicted-i)))

(define (result-matrix-formatted C)
  (let* ([column-width (apply max (map string-length (hash-keys (confusion-matrix-values C))))]
         [label (λ (str) (make-label str column-width))]
         [datum (λ (dat) (make-datum dat column-width))]
         [labels (make-hash (hash-map (confusion-matrix-values C) (lambda (k v) (cons v k))))])
    (list*
      (list* (label "true ω pred") (for/list ([i (range (hash-count labels))]) (label (hash-ref labels i))))
      (for/list ([i (range (hash-count labels))])
        (list* (list* (label (hash-ref labels i)))
          (map datum (vector->list (vector-ref (confusion-matrix-results C) i))))))))

(define (make-datum datum len)
  (let ([str (cond
               [(string? datum) datum]
               [(number? datum) (number->string datum)]
               [(symbol? datum) (symbol->string datum)]
               [else "???"])])
    (string-append (make-string (- len (string-length str)) #\space) str " ")))

(define (make-label str len)
  (string-append str (make-string (- len (string-length str)) #\space) " "))
