(ns shorturl.core-test
  (:use clojure.test
        shorturl.core))

(defn my-fixture [f]
  ;; setup
  (reset! counter 10000)
  (reset! urls {})
  (f )
  ;; tear-down
  )

(use-fixtures :each my-fixture)

(deftest test-shorten
    (is ( = "7pt" ( shorten "http://www.google.com"))))
