;; Evan Hmisty

;; the activation function
;; (activation '(1 2 3) '(4 5 6)) => 1*4 + 2*5 + 3*6
(define (activation weights inputs)
  (if (or (null? weights) (null? inputs))
    0
    (+ (* (car weights) (car inputs)) 
       (activation (cdr weights) (cdr inputs)))))

;; the create-binary-neuron function
;; (define n (create-binary-neuron 10 '(1 2 3)))
;; (n '(4 5 6)) => 1
;; (n '(1 2 0)) => 0
(define (create-binary-neuron threshold weights)
  (lambda (inputs)
    (if (>= (activation weights inputs) threshold)
      1
      0)))

;; the create-triple-neuron function
;; (define n (create-triple-neuron 10 50 '(1 2 3)))
;; (n '(4 5 6)) => 0
;; (n '(1 2 -3)) => -1
;; (n '(1 20 10)) => 1
(define (create-triple-neuron threshold1 threshold2 weights)
  (let ((threshold-high (max threshold1 threshold2))
        (threshold-low  (min threshold1 threshold2)))
  (lambda (inputs)
    (if (>= (activation weights inputs) threshold-high)
      1
      (if (<= (activation weights inputs) threshold-low)
        -1
        0)))))

;; the create-sigmoidal-neuron function (sigmoid)
;; (define n (create-sigmoidal-neuron '(1 2 3)))
;; (n '(0 0 0)) => 1/2
;; (n '(100 0 0)) => 1.
;; (n '(-100 0 0)) => 0.
(define (create-sigmoidal-neuron weights)
  (lambda (inputs)
    (/ 1 (+ 1 (exp (- (activation weights inputs)))))))

;; the adjust-weight function
;; => new weights
(define (adjust-weight learning-speed desire sigmoidal-neuron weights inputs)
  (let ((output (sigmoidal-neuron inputs)))
    (map
      (lambda (x w) 
        (+ w (* -2 learning-speed (- output desire) output (- 1 output) x)))
      inputs weights)))

;; create-network
;; replace layers definition to layered neurons
(define (create-network layers)
  (map (lambda (layer)
         (map (car layer) (cdr layer))) layers))

;; calculate-network
;; calculate values of neurons with given inputs
(define (calculate-network network inputs)
  (define (cn layers previous-layer-outputs)
    (cons (map (lambda (f x) (f x)) (car layers) previous-layer-outputs)
          (cn (cdr layers) inputs)))



;; train three neuron with (1, 0.25, -0.5) => (1, -1, 0)
(define learning-speed 10000)
(define inputs '(1 0.25 -0.5))
(define desires '(1 -1 0))
(define layers 
  `((,create-sigmoidal-neuron (0 0) (0 0) (0 0)) ;; layer 3: 3 neurons
    (,create-sigmoidal-neuron (0 0 0) (0 0 0))   ;; layer 2: 2 neurons
    (,create-sigmoidal-neuron (0) (0) (0))))     ;; layer 1: 3 neurons
(define network (create-network layers))



;(define c 100)
;(define (train1 network c)
;  (if (> c 0)
;  (let ((nw1 (adjust-weight learning-speed (first desires) n1 w1 inputs))
;        (nw2 (adjust-weight learning-speed (second desires) n2 w2 inputs))
;        (nw3 (adjust-weight learning-speed (third desires) n3 w3 inputs)))
;    (let ((nn1 (create-sigmoidal-neuron nw1))
;          (nn2 (create-sigmoidal-neuron nw2))
;          (nn3 (create-sigmoidal-neuron nw3)))
;      (train3 (- c 1) nn1 nn2 nn3 nw1 nw2 nw3)))
;  (list n1 n2 n3)))
;

