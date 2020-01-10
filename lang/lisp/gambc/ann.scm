#!env gsi-script
;; modified from http://www.idubeda.es/article/learning-lisp-while-implementing-an-artificial-neural-network-ann-part-0-introduction

;; cross-product of two vectors x and y
;; (scalar-product '(1 2 3) '(4 5 6)) => 1*4 + 2*5 + 3*6
(define (scalar-product x y)
  (if (null? x)
	0
	(+ (* (car x) (car y))
	   (scalar-product (cdr x) (cdr y)))))

;; Function that simulates the work of a Threshold Logic Unit (TLU).
;; Inputs:
;; inputs   Inputs to the TLU: a list with numbers representing
;;          the values received by the TLU. The length of this list must be
;;          equal to the number of weights.
;; Output:
;;  [0|1]
(define (TLU inputs)
(let ((weights '(1.0 1.0))
	  (threshold 0.5))
	(let ((s (scalar-product weights inputs)))
	  (if (>= s threshold)
		1
		0))))

;; Unit tests for the TLU function
;(= (TLU '(1 1)) 1)
;(= (TLU '(1 0)) 1)
;(= (TLU '(0 1)) 1)
;(= (TLU '(0 0)) 0)

;;; Function that returns a function that behaves like
;;;  a Threshold Logic Unit.
;; Inputs:
;; properties  Properties of the TLU: a list with numbers representing
;;           the threshold value and the input weights
;;          (threshold is the first element of the list).
(define (create-TLU properties)
  (let ((threshold (car properties))
        (weights (cdr properties)))
    (lambda (inputs)
      (if (>= (scalar-product weights inputs)
             threshold)
          1
          0))))
;; (define n1 (create-TLU '(0.5 1.0 1.0)))
;; (n1 '(1 0))

