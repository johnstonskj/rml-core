#lang racket
;;
;; Racket Machine Learning - K-NN.
;;
;; ~ Simon Johnston 2018.


(provide
 (contract-out

  [make-result-matrix
   (-> data-set? confusion-matrix?)]

  [record-result
   (-> confusion-matrix? any/c any/c confusion-matrix?)]

  [result-matrix
   (-> confusion-matrix? list?)]))

;; ---------- Requirements

(require rml/data
         math/array)

;; ---------- Internal types

(struct confusion-matrix
 (values
  Ω
  results))

;; ---------- Implementation

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

(define (result-matrix C)
  (let ([labels (make-hash (hash-map (confusion-matrix-values C) (lambda (k v) (cons v k))))])
    (list*
      (list* "true ω pred" (for/list ([i (range (hash-count labels))]) (hash-ref labels i)))
      (for/list ([i (range (hash-count labels))])
        (list* (list* (hash-ref labels i))
          (vector->list (vector-ref (confusion-matrix-results C) i)))))))
