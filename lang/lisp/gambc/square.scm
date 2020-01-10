#!gsi-script -:d0
(define (main arg)
  (display "hello")
  (newline)
  (pretty-print (expt (string->number arg) 2)))
;;; TODO 
;;; known issue
;;; $ gsi square 30
;;; hello
;;; 900
;;; $ gsc -exe square
;;; $ ./square 30
;;; NO OUTPUT! BECAUSE main IS NOT CALLED!
