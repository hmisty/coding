; lein repl
; => (load "cloapl")
; => (run-tests)
;
(ns cloapl
  {:author hmisty}
  (:require [clojure.string :as string])
  (:use clojure.test))

; APL expr to S epxr
; left-value function right-value
; => (function left-value right-value)
; => (->> right-value (function left-value))

(defn func?
  "Returns true if argument is a function or a symbol that resolves to a function or macro."
  [x]
  (if (symbol? x)
    (when-let [v (resolve x)]
      (when-let [value (get-possibly-unbound-var v)]
        (or (fn? value)
            (:macro (meta v)))))
    (fn? x)))

(defn transform-2
  "Associates the function and its left-value (left-value on the right), if any
  e.g. f1 f2 x2 f3 x3 f4 => f1 (f2 x2) (f3 x3) f4
  "
  [forms]
  (let [x (first forms)
        xs (rest forms)
        y (second forms)
        ys (drop 2 forms)]
    (cond (nil? x) '()
          (nil? y) (list x)
          (func? y) (cons x (transform-2 xs))
          :else (concat (list (list x y)) (transform-2 ys)))))

(defmacro transform-1
  "Threads the transformed expressions
  e.g. f4 x3 f3 x2 f2 f1 x1 => ->> x1 f1 (f2 x2) (f3 x3) f4
  "
  [& forms]
  (let [forms' (reverse forms)
        x (first forms')
        xs (rest forms')
        y (second forms')]
    (if (nil? y) x
      (concat `(->> ~x) (transform-2 xs)))))

(defmacro <-
  "APL expr evaluator
  e.g. (<- (2 +| 1) *| 3 || 6) => 3/2
  "
  [& forms]
  (concat `(transform-1) (for [x forms] (if (list? x) (concat `(<-) x) x))))

; functions

(defmacro vectorize
  [f]
  `(fn
     ([x#] (if (coll? x#) (map ~f x#) (~f x#)))
     ([y# x#] (if (coll? x#)
                (if (coll? y#) 
                  (map ~f y# x#)
                  (map ~f (repeat y#) x#))
                (if (coll? y#)
                  (map ~f y# (repeat x#))
                  (~f y# x#))))))

(def +| (vectorize +))
(def -| (vectorize -))
(def *| (vectorize *))
(def || (vectorize /))

; test cases

; page 6
; 65.35 + 35.65
(is (= 101.
       (+| 65.35 35.65)
       (<- 65.35 + 35.65)
       (<- 65.35 +| 35.65)))

; 14 - 9
(is (= 5
       (-| 14 9)
       (<- 14 - 9)
       (<- 14 -| 9)))

; - 7
(is (= -7
       (-| 7)
       (<- - 7)
       (<- -| 7)))

; page 7
; 3 x 4 + 6
(is (= 30
       (*| 3 (+| 4 6))
       (<- 3 * 4 + 6)
       (<- 3 *| 4 +| 6)))

; (3 x 4) + 6
(is (= 18
       (+| (*| 3 4) 6)
       (<- (3 * 4) + 6)
       (<- (3 *| 4) +| 6)))

; 14 - 6 - 5 - 3 - 7
(is (= 17
       (-| 14 (-| 6 (-| 5 (-| 3 7))))
       (<- 14 - 6 - 5 - 3 - 7)
       (<- 14 -| 6 -| 5 -| 3 -| 7)))

; true or true
(is (= true
       (or true true)
       (<- true or true)))

; true and (false and true) or true or false
(is (= true
       (and true (or (and false true) (or true false)))
       (<- true and (false and true) or true or false)))

; not false or not false
(is (= false
       (not (or false (not false)))
       (<- not false or not false)))

; page 8
; 20.5 = 41 / 2
(is (= true
       (= 41/2 (/ 41 2))
       (<- 41/2 = 41 / 2)))

; 101 < 200 - 100
(is (= false
       (< 101 (- 200 100))
       (<- 101 < 200 - 100)))

; 27.3 > 39.31
(is (= false
       (> 27.3 39.31)
       (<- 27.3 > 39.31)))

(defn- circle
  [f n]
  (cond
    (= f 1) (Math/sin n)
    (= f 2) (Math/cos n)
    (= f 3) (Math/tan n)))

(def circle| (vectorize circle))

; 1 circle 3.14159
(is (= 2.65358979335273E-6
       (circle| 1 3.14159)
       (<- 1 circle| 3.14159)))

; page 10
; 1 2 3 + 4 6 8
(is (= [5 8 11]
       (+| [1 2 3] [4 6 8])
       (<- [1 2 3] +| [4 6 8])))

; page 11
; - 3 4 5
(is (= [-3 -4 -5]
       (-| [3 4 5])
       (<- -| [3 4 5])))

; 1 circle .1 .2 .3
(is (= [0.09983341664682815 0.19866933079506122 0.29552020666133955]
       (circle| 1 [0.1 0.2 0.3])
       (<- 1 circle| [0.1 0.2 0.3])))

; page 12
(def NAME "GRAEME ROBERTSON")
(def ADDRESS "APL4.NET")

(defn reverse|
  [x]
  (if (string? x) (string/reverse x) (reverse x)))

; reverse NAME
(is (= "NOSTREBOR EMEARG"
       (<- reverse| NAME)))

(defn take|
  [n x]
  (if (string? x) (subs x 0 n) (take n x)))

(defn drop|
  [n x]
  (if (string? x) (subs x n) (drop n x)))

; 7 take NAME
(is (= "GRAEME "
       (<- 7 take| NAME)))

(defn cat|
  " catenate
  "
  [y x]
  (if (string? x) (string/join [y x]) (concat y x)))

; (6 take NAME) cat "@" cat ADDRESS
(is (= "GRAEME@APL4.NET"
       (<- (6 take| NAME) cat| "@" cat| ADDRESS)))

(defn shape|
  ([x] (count x))
  ([n x]
   (if (coll? n)
     ()
     (take| n 
                (apply (if (string? x) str concat)
                       (repeat (/ n (count x)) x))))

; 4 shape NAME
(is (= "APL4"
       (<- 4 shape| ADDRESS)))

; 40 shape ADDRESS
(is (= "APL4.NETAPL4.NETAPL4.NETAPL4.NETAPL4.NET"
       (<- 40 shape| ADDRESS)))

; reverse 50 shape NAME cat " "
(is (= "NOSTREBOR EMEARG NOSTREBOR EMEARG NOSTREBOR EMEARG"
       (<- reverse| 50 shape| NAME cat| " ")))


; let's go
(run-tests)

