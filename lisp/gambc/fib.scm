;;; usage:
;;; $ gsi fib
;;; $ gsc -exe fib.scm //windows
(define (fib n) 
  (if (< n 2) 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(println (fib 10))

