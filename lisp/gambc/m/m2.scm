; File: "m2.scm"
;(c-declare "extern int power_of_2 ();")
(c-declare #<<EOL

extern int power_of_2 ();

EOL
)

(define pow2 (c-lambda (int) int "power_of_2"))
(define (twice x) (cons x x))

